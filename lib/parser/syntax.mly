%{
  let make ~b ~e kind =
    Node.{kind; span = Span.create_from_lex_pos ~b ~e}
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV
%token EQ
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COLON SEMICOLON DOT
%token KEYWORD_DEF KEYWORD_VAR KEYWORD_NEW
%token EOF

%left PLUS MINUS
%left TIMES DIV
%left DOT
%nonassoc UMINUS

%start <Node.t> main

%on_error_reduce
  stmt_var
  expr
  compound_kv

%%

main:
    n = stmts EOF
    { n }

stmts:
    xs = list(stmt)
    { make (Node.Stmts xs) ~b:$startpos ~e:$endpos }

stmt:
    stmt_var { $1 }

stmt_var:
    KEYWORD_VAR id = ID EQ e = expr SEMICOLON
    { make (Node.StmtVar (id, e)) ~b:$startpos ~e:$endpos }

expr:
    e1 = expr PLUS e2 = expr
    { make (Node.ExprBin ("+", e1, e2)) ~b:$startpos ~e:$endpos }
  | e1 = expr MINUS e2 = expr
    { make (Node.ExprBin ("-", e1, e2)) ~b:$startpos ~e:$endpos }
  | e1 = expr TIMES e2 = expr
    { make (Node.ExprBin ("*", e1, e2)) ~b:$startpos ~e:$endpos }
  | e1 = expr DIV e2 = expr
    { make (Node.ExprBin ("/", e1, e2)) ~b:$startpos ~e:$endpos }
  | e = expr DOT id = ID
    { make (Node.ExprSelection (e, id)) ~b:$startpos ~e:$endpos }
  | MINUS e = expr %prec UMINUS
    { make (Node.ExprUnary ("-", e)) ~b:$startpos ~e:$endpos }
  | i = INT
    { make (Node.LitInt i) ~b:$startpos ~e:$endpos }
  | id = ID
    { make (Node.Id id) ~b:$startpos ~e:$endpos }
  | LPAREN e = expr RPAREN
    { e }
  | LBRACE kvs = separated_list(SEMICOLON, compound_kv) RBRACE
    { make (Node.ExprCompound kvs) ~b:$startpos ~e:$endpos }

compound_kv:
    id = ID COLON e = expr
    {
      let id_node = make (Node.Id id) ~b:$startpos(id) ~e:$endpos(id) in
      make (Node.ExprCompoundKV (id_node, e)) ~b:$startpos ~e:$endpos
    }
