type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | CONCAT
  | QUOTE
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | COMMA
  | LCURL
  | RCURL
  | LSQR
  | RSQR
  | HEAD
  | TAIL
  | ISEMPTY
  | FIRST
  | SECOND
  | INTV of (int)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
