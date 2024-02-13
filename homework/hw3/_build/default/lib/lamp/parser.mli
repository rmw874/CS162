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
  | AT
  | FIX
  | IS
  | LBRACE
  | RBRACE
  | FST
  | SND
  | TRUE
  | FALSE
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
  | TYBOOL
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

val letbind :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * Ast.expr)
val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
