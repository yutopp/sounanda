{
  open Syntax

  exception LexerError of char * Lexing.position * Lexing.position
}

let blank = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'
let numeric_10 = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | blank           { token lexbuf }
  | newline         { Lexing.new_line lexbuf; token lexbuf }

  | eof             { EOF }

  | numeric_10 as i { INT (int_of_string i) }

  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | '='             { EQ }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ':'             { COLON }
  | ';'             { SEMICOLON }
  | '.'             { DOT }
  | "def"           { KEYWORD_DEF }
  | "var"           { KEYWORD_VAR }
  | "new"           { KEYWORD_NEW }
  | id as s         { ID s }
  | _ as c          { raise (LexerError (c, (Lexing.lexeme_start_p lexbuf), (Lexing.lexeme_end_p lexbuf))) }
