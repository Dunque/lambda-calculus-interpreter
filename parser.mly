
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
  | LCURL recordTerm RCURL DOT STRINGV
      { TmFindRecord ($2, $5) }
  | pairTerm
      { $1 }


pairTerm :
  | LCURL term COMMA term RCURL 
      { TmPair ($2,$4) }
  | LCURL term COMMA term RCURL FIRST
      { TmFirst ($2,$4) }
  | LCURL term COMMA term RCURL SECOND
      { TmSecond ($2,$4) }
      
recordTerm :
    STRINGV EQ term
      { [($1,$3)] }
    | STRINGV EQ term COMMA recordTerm
      { ($1,$3)::$5 }

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

