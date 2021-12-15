
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token CONCAT

%token QUOTE
%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token COMMA
%token LCURL
%token RCURL

%token LIST
%token LSQR
%token RSQR
%token CONST
%token NIL
%token HEAD
%token TAIL
%token ISNIL

%token FIRST
%token SECOND

%token <int> INTV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s:
    STRINGV EQ term EOF
        { Bind ($1, $3) }
    | term EOF
        { Eval $1 }
        
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix ( TmAbs ($2, $4, $6)), $8) }
  | CONCAT term term
      { TmConcat ($2,$3) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | CONCAT appTerm appTerm
      { TmConcat ($2,$3) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | QUOTE STRINGV QUOTE 
      { TmStr $2 }
  | LCURL recordTerm RCURL
      { TmRecord $2 }
  | pairTerm
      { $1 }
  | atomicTerm FIRST
      { TmFirst $1 }
  | atomicTerm SECOND
      { TmSecond $1 }
  | atomicTerm DOT STRINGV
      { TmFindRecord ($1, $3) }
  | CONST LSQR ty RSQR atomicTerm atomicTerm
      { TmConst ($3,$5,$6) }
  | NIL LSQR ty RSQR
      { TmNil $3 }
  | HEAD LSQR ty RSQR atomicTerm
      { TmHead ($3,$5) }
  | TAIL LSQR ty RSQR atomicTerm
      { TmTail ($3,$5) }
  | ISNIL LSQR ty RSQR atomicTerm
      { TmIsNil ($3,$5) }


pairTerm :
  | LCURL term COMMA term RCURL 
      { TmPair ($2,$4) }
      
recordTerm :
    STRINGV EQ term
      { [($1,$3)] }
    | STRINGV EQ term COMMA recordTerm
      { ($1,$3)::$5 }

listTerm :
    term
      { [$1] }
    | term COMMA listTerm
      { $1::$3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRINGV
      { TyStr }
  | LCURL ty COMMA ty RCURL 
      { TyPair ($2,$4) }
  | LCURL recordTy RCURL
      { TyRecord $2 }
  | LIST ty
      { TyList $2 }
  | NIL ty
      { TyNil $2 }
/*   | LSQR ty RSQR
      { TyList $2 }
  | LSQR RSQR
      { TyList TyEmpty } */

recordTy :
    STRINGV EQ ty
      { [($1,$3)] }
    | STRINGV EQ ty COMMA recordTy
      { ($1,$3)::$5 }