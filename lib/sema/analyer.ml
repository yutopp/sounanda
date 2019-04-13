open! Base

module Ty = struct
  type t =
    | Int
    | Compound of (string * t) list
    | Unit
    | Undef
  [@@deriving sexp_of]
end

(* venv *)
module Env = struct
  type t = {
    symbols: venv_t Map.M(String).t;
  }
  and venv_t = {
    ty: Ty.t;
  }
  [@@deriving sexp_of]

  let empty () =
    {
      symbols = Map.empty (module String);
    }

  let update e ~name ~ty =
    let v = {
      ty
    } in
    let symbols' = Map.set e.symbols ~key:name ~data:v in
    {e with symbols = symbols'}

  let find e name =
    Map.find e.symbols name
end

module Node = struct
  type t =
    {
      kind: kind_t;
      span: Sounanda_parser.Span.t;
      ty: Ty.t;
    }

  and kind_t =
    | Stmts of t list
    | StmtVar of string * t
    | ExprUnary of string * t
    | ExprBin of string * t * t
    | ExprSelection of t * string
    | ExprCompound of (string * t) list
    | LitInt of int
    | Id of string
    | Unknown
  [@@deriving sexp_of]

  (* TODO: unify logic *)
  let rec find_node_kind k span last =
    match k with
    | Stmts xs ->
       let elems = xs |> List.map ~f:(fun x -> find_node' x span last) |> List.filter_opt in
       min_nodes elems last

    | StmtVar (_, e) ->
       find_node' e span last

    | ExprUnary (_, e) ->
       find_node' e span last

    | ExprBin (_, lhs, rhs) ->
       let lhs_last = find_node' lhs span last in
       let rhs_last = find_node' rhs span last in
       let elems = List.filter_opt [lhs_last; rhs_last] in
       min_nodes elems last

    | ExprSelection (e, _) ->
       find_node' e span last

    | ExprCompound _ ->
       last

    | Id _ ->
       last

    | LitInt _ ->
       last

    | Unknown ->
       last

  and min_nodes elems last =
    let module Span = Sounanda_parser.Span in
    List.fold_left elems
                   ~f:(fun acc r ->
                     Some (match acc with
                           | Some l ->
                              if phys_equal (Span.min l.span r.span) l.span then l else r
                           | None ->
                              r
                          )
                   )
                   ~init:last

  and find_node' current span (last : t option) =
    let module Span = Sounanda_parser.Span in

    let l =
      match Span.is_enclosed span current.span with
      | true ->
         Some (match last with
               | Some last_t ->
                  if phys_equal (Span.min current.span last_t.span) current.span then
                    current
                  else
                    last_t
               | None ->
                  current
              )

      | false ->
         last
    in
    find_node_kind current.kind span l

  and find_node t span =
    let module Span = Sounanda_parser.Span in
    match Span.is_wrapped span t.span with
    | true ->
       find_node' t span None
    | false ->
       None

end

let rec analyze node env ds : (Node.t * Env.t * 'a list)=
  let module PNode = Sounanda_parser.Node in
  let module D = Sounanda_parser.Parser.Diagnostics in

  match node with
  | PNode.{kind = Stmts stmts; span} ->
     let (stmts, env', ds') =
       List.fold_left ~f:(fun (sx, env, ds) stmt ->
                        let (stmt', env', ds') = analyze stmt env ds in
                        (stmt' :: sx, env', ds')
                      )
                      ~init:([], env, ds)
                      stmts
     in
     (Node.{kind = Stmts (List.rev stmts); span; ty = Ty.Unit}, env', ds')

  | PNode.{kind = StmtVar (name, e); span} ->
     let (n', env', ds') = analyze e env ds in
     let env' = Env.update env' ~name:name ~ty:n'.Node.ty in
     (Node.{kind = StmtVar (name, n'); span; ty = Ty.Unit}, env', ds')

  | PNode.{kind = ExprUnary (op, e); span} ->
     let (n', _, ds') = analyze e env ds in
     let ty = match n'.Node.ty with
       | Ty.Int -> Ty.Int
       | _ -> Ty.Undef
     in
     (Node.{kind = ExprUnary (op, n'); span; ty}, env, ds')

  | PNode.{kind = ExprBin (op, lhs, rhs); span} ->
     let (lhs', _, ds') = analyze lhs env ds in
     let (rhs', _, ds'') = analyze rhs env ds' in
     let ty = match lhs'.Node.ty, rhs'.Node.ty with
       | Ty.Int, Ty.Int -> Ty.Int
       | _ -> Ty.Undef
     in
     (Node.{kind = ExprBin (op, lhs', rhs'); span; ty}, env, ds'')

  | PNode.{kind = ExprSelection (e, name); span} ->
     let (e', _, ds') = analyze e env ds in
     let (ty, ds_c) = match e'.Node.ty with
       | Ty.Compound ty_kvs ->
          let kvopt =
            List.find ty_kvs
                      ~f:(fun (k, _) -> phys_equal k name)
          in
          begin match kvopt with
          | Some (_, ty) ->
             (ty, [])
          | None ->
             let d = D.create_s span in
             (Ty.Undef, [d])
          end
       | _ ->
          let d = D.create_s span in
          (Ty.Undef, [d])
     in
     (Node.{kind = ExprSelection (e', name); span; ty}, env, ds_c @ ds')

  | PNode.{kind = ExprCompound comps; span} ->
     let rel =
       List.map ~f:(fun node ->
                  match node with
                  | PNode.{kind = ExprCompoundKV (id, expr); span} ->
                     let name = match id with
                       | PNode.{kind = Id name; _} -> name
                       | _ -> failwith ""
                     in
                     let (v, _, ds) = analyze expr env [] in
                     ((name, v.Node.ty), (name, v), ds)
                  | _ ->
                     failwith ""
                )
                comps
     in
     let (ty_kvs, kvs, ds_list) = List.unzip3 rel in
     let ty = Ty.Compound ty_kvs in
     (Node.{kind = ExprCompound kvs; span; ty}, env, (List.join ds_list) @ ds)

  | PNode.{kind = LitInt v; span} ->
     (Node.{kind = LitInt v; span; ty = Ty.Int}, env, ds)

  | PNode.{kind = Id name; span} ->
     let vo = Env.find env name in
     let (ty, ds') = match vo with
       | Some v ->
          (v.Env.ty, [])
       | None ->
          let d = D.create_s span in
          (Ty.Undef, [d])
     in
     (Node.{kind = Id name; span; ty}, env, ds' @ ds)

  | _ ->
     failwith ""

let analyze node =
  let env = Env.empty () in
  analyze node env []
