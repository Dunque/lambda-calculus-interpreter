
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyStr
  | TyPair of ty * ty
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyEmpty
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmStr of string
  | TmConcat of term * term
  | TmPair of term * term
  | TmFirst of term
  | TmSecond of term
  | TmRecord of (string * term) list
  | TmFindRecord of term * string
  | TmList of term list
  | TmEmpty
  | TmHead of term
  | TmTail of term
  | TmIsEmptyList of term
;;

type context = (string * (term * ty)) list
;;

type command = 
    Eval of term
    | Bind of string * term
;;

val emptyctx : context;;
val addbinding : context -> string -> term -> ty -> context;;
val getbinding : context -> string -> (term * ty);;


val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : bool -> context -> term -> term;;

