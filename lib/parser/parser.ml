open! Base

module I = Syntax.MenhirInterpreter

let adhoc_dummy_table =
  Map.empty (module Int)
  |> Map.add_exn ~key:1 ~data:(Syntax.ID "_")
  |> Map.add_exn ~key:2 ~data:(Syntax.EQ)
  |> Map.add_exn ~key:3 ~data:(Syntax.ID "_")
  |> Map.add_exn ~key:8 ~data:(Syntax.ID "_")
  |> Map.add_exn ~key:14 ~data:(Syntax.ID "_")
  |> Map.add_exn ~key:31 ~data:(Syntax.SEMICOLON)
  |> Map.add_exn ~key:36 ~data:(Syntax.KEYWORD_VAR)
  |> Map.add_exn ~key:25 ~data:(Syntax.SEMICOLON)
  |> Map.add_exn ~key:26 ~data:(Syntax.ID "_")
  |> Map.add_exn ~key:11 ~data:(Syntax.SEMICOLON)
  |> Map.add_exn ~key:7 ~data:(Syntax.COLON)

module Sup = struct
  type t = {
    lexbuf: Lexing.lexbuf;
    supplier: I.supplier;
    mutable tokens: (Syntax.token * Lexing.position * Lexing.position) list;
    mutable index: int;
  }

  let create ~lexbuf =
    {
      lexbuf;
      supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf;
      tokens = [];
      index = 0;
    }

  let top sup : (Syntax.token * Lexing.position * Lexing.position) =
    List.hd_exn sup.tokens

  let push sup tup : unit =
    sup.tokens <- tup :: sup.tokens

  let next sup =
    let tup =
      if (List.length sup.tokens) <= sup.index then
        let tup = sup.supplier () in
        sup.tokens <- tup :: sup.tokens;
        tup
      else
        List.nth_exn sup.tokens ((List.length sup.tokens) - sup.index - 1)
    in
    sup.index <- sup.index + 1;

    let (tok, s, e) = tup in

    tup

  let get sup : I.supplier =
    let f () =
      next sup
    in
    f
end

module Diagnostics = struct
  type t = {
    span: Span.t;
  }
  [@@deriving sexp_of]

  let create_s span =
    {
      span
    }

  let create (b, e) =
    create_s (Span.create_from_lex_pos ~b ~e)
end

exception CannotRecoverly of string * (Lexing.position * Lexing.position)

let succeed dx (v : Node.t) : (Node.t option * Diagnostics.t list) =
  (Some v, dx)

let rec fail dx sup inputneeded checkpoint : (Node.t option * Diagnostics.t list) =
  match checkpoint with
  | I.HandlingError env ->
     let (pos, _) as positions = I.positions env in

     let d = Diagnostics.create positions in
     let dx = d :: dx in

     let dummy_token =
       match I.top env with
       | Some (I.Element (state, _, _, _)) ->
          let state_num = I.number state in
          let dummy_token_opt =
            Stdio.eprintf "%d\n" state_num;
            Stdio.Out_channel.flush Stdio.stderr;

            Map.find adhoc_dummy_table state_num
          in
          begin match dummy_token_opt with
          | Some tok ->
             tok

          | None ->
             let msg = Printf.sprintf "state = %d" state_num in
             raise (CannotRecoverly (msg, positions))
          end

       | _ ->
          raise (CannotRecoverly ("stack is empty", positions))
     in
     assert (I.acceptable inputneeded dummy_token pos);

     let current = Sup.top sup in
     let inserted = (dummy_token, pos, pos) in

     Sup.push sup inserted;
     Sup.push sup current;

     I.loop_handle_undo (succeed dx) (fail dx sup) (Sup.get sup) inputneeded

  | _ ->
     failwith ""

let entry sup start =
  I.loop_handle_undo (succeed []) (fail [] sup) (Sup.get sup) start

let parse_from_string text =
  let lexbuf = Lexing.from_string text in
  try
    let sup = Sup.create ~lexbuf in
    let start = (Syntax.Incremental.main lexbuf.Lexing.lex_curr_p) in
    entry sup start
  with
  | Lexer.LexerError (c, b, e) ->
     let d = Diagnostics.create (b, e) in
     (None, [d])
  | CannotRecoverly (msg, (b, e)) ->
     let d = Diagnostics.create (b, e) in
     (None, [d])
