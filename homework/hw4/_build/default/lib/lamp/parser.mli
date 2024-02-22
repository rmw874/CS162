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
  | TRUE
  | FALSE
  | TYBOOL
  | FST
  | SND
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | WITH
  | LAMBDA
  | NIL
  | CONS
  | SEMI
  | SHARP
  | TYINT
  | TYLIST
  | THINARROW
  | COLON
  | LBRACK
  | RBRACK
  | CLET
  | CPRINT
  | CCLEAR
  | CLOAD
  | CSAVE
  | CPLUSMETA
  | CMINUSMETA
  | PLUS
  | SUB
  | TIMES
  | APP
  | NUMBER of (int)
  | ID of (string)
  | FILE of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cmd.t
val commands :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cmd.t list
val ty :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ty
