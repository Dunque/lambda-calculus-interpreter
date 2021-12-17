(*
Roi Santos Ríos
Miguel Pérez Gómara
*)

{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "concat"    { CONCAT }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "List"      { LIST }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "nil"       { NIL }
  | "isNil"     { ISNIL }
  | "const"     { CONST }
  | "\""        { QUOTE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ","         { COMMA }
  | "{"         { LCURL }
  | "}"         { RCURL }
  | "["         { LSQR }
  | "]"         { RSQR }
  | ".1"        { FIRST }
  | ".2"        { SECOND }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

