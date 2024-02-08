type token =
  | EOF
  | FUN
  | REC
  | MATCH
  | BAR
  | END
  | GT
  | EQ
  | LT
  | LPAREN
  | RPAREN
  | DOT
  | COMMA
  | TRUE
  | FALSE
  | AND
  | OR
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | WITH
  | LAMBDA
  | NIL
  | CONS
  | TYINT
  | TYLIST
  | THINARROW
  | COLON
  | LBRACK
  | RBRACK
  | PLUS
  | SUB
  | TIMES
  | APP
  | NUMBER of (int)
  | ID of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
