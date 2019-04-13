open! Base

type t = (pos_t * pos_t)
[@@deriving sexp]

and pos_t = {
  lnum: int;
  cnum: int;
}
[@@deriving sexp]

let is_wrapped inner outer =
  let (ib, ie) = inner in
  let (ob, oe) = outer in
  if oe.lnum < ib.lnum then
    false
  else if oe.lnum = ib.lnum && oe.cnum < ib.cnum then
    false
  else if ie.lnum < ob.lnum then
    false
  else if ie.lnum = ob.lnum && ie.cnum < ob.cnum then
    false
  else
    true

let is_enclosed inner outer =
  let (ib, ie) = inner in
  let (ob, oe) = outer in
  if ob.lnum <= ib.lnum && ie.lnum <= oe.lnum then
    let left_covered = if ob.lnum = ib.lnum then ob.cnum <= ib.cnum else true in
    let right_covered = if ie.lnum = oe.lnum then ie.cnum <= oe.cnum else true in
    left_covered && right_covered
  else
    false

let max lhs rhs =
  if is_enclosed lhs rhs then
    let (lb, le) = lhs in
    let (rb, re) = rhs in
    if lb.lnum < rb.lnum then
      lhs
    else if le.lnum < re.lnum then
      rhs
    else
      if lb.cnum < rb.cnum then
        lhs
      else if le.cnum < re.cnum then
        rhs
      else
        rhs (* same as lhs *)
  else
    failwith ""

let min lhs rhs =
  if is_enclosed lhs rhs then
    let (lb, le) = lhs in
    let (rb, re) = rhs in
    if lb.lnum < rb.lnum then
      rhs
    else if le.lnum < re.lnum then
      lhs
    else
      if lb.cnum < rb.cnum then
        rhs
      else if le.cnum < re.cnum then
        lhs
      else
        rhs (* same as lhs *)
  else
    rhs

let create_pos ~line ~column =
  {
    lnum = line;
    cnum = column;
  }

let to_pos p =
  create_pos ~line:(p.Lexing.pos_lnum)
             ~column:(p.Lexing.pos_cnum - p.Lexing.pos_bol)

let create ~b ~e =
  (b, e)

let create_from_lex_pos ~b ~e =
  create ~b:(b |> to_pos) ~e:(e |> to_pos)

let from_lexbuf lexbuf =
  create_from_lex_pos ~b:(Lexing.lexeme_start_p lexbuf) ~e:(Lexing.lexeme_end_p lexbuf)

let to_string span =
  let (b, e) = span in
  if b.lnum = e.lnum then
    Printf.sprintf "Line %d, charactor %d-%d"
                   b.lnum b.cnum e.cnum
  else
    Printf.sprintf "Line %d, charactor %d to Line %d, charactor %d"
                   b.lnum b.cnum e.lnum e.cnum
