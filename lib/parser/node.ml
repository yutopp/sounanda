open! Base

type t =
  {
    kind: kind_t;
    span: Span.t;
  }

and kind_t =
  | Stmts of t list
  | StmtVar of string * t
  | ExprUnary of string * t
  | ExprBin of string * t * t
  | ExprSelection of t * string
  | ExprCompoundKV of t * t
  | ExprCompound of t list
  | LitInt of int
  | Id of string
[@@deriving sexp_of]

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

  | ExprCompoundKV (k, v) ->
     let k_last = find_node' k span last in
     let v_last = find_node' v span last in
     let elems = List.filter_opt [k_last; v_last] in
     min_nodes elems last

  | ExprCompound kvs ->
     let elems = kvs |> List.map ~f:(fun x -> find_node' x span last) |> List.filter_opt in
     min_nodes elems last

  | Id _ ->
     last

  | LitInt _ ->
     last

and min_nodes elems last =
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
  Stdio.printf "current <= span? %b; C => %s\n"
               (Span.is_enclosed span current.span)
               (current |> sexp_of_t |> Sexp.to_string_hum ~indent:2);
  let (sb, se) = span in
  Stdio.printf "span = (%d, %d) - (%d, %d)\n" sb.Span.lnum sb.Span.cnum se.Span.lnum se.Span.cnum;

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
  Stdio.printf "SELECTED => %s\n"
               (l |> Option.map ~f:(fun n -> n |> sexp_of_t |> Sexp.to_string_hum ~indent:2) |> Option.value ~default:"NONE");

  find_node_kind current.kind span l

and find_node t span =
  match Span.is_wrapped span t.span with
  | true ->
     Stdio.printf "F => %s\n" (t |> sexp_of_t |> Sexp.to_string_hum ~indent:2);
     find_node' t span None
  | false ->
     None
