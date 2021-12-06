
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyStr
  | TyPair of ty * ty
  | TyRecord of ty list
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
;;

type command = 
    Eval of term
    | Bind of string * term

type context = (string * (term * ty)) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x term bind =
  (x, (term, bind)) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyStr ->
    "String"
  | TyPair (ty1, ty2) ->
    "{" ^ string_of_ty ty1 ^ "," ^ string_of_ty ty2 ^ "}"
  | TyRecord t ->
    match t with
      tm::tail -> string_of_ty tm ^ "," ^ string_of_ty (TyRecord tail)
     | _ -> ""
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try snd (getbinding ctx x) with
      Not_found -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x t2 tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x t1 tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))

  | TmStr _ ->
      TyStr
  
  | TmConcat (t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match (tyT1,tyT2) with
          (TyStr,TyStr) -> TyStr
        | _ -> raise (Type_error "arguments must be strings"))

  | TmPair (t1,t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
      TyPair (tyT1,tyT2)

  | TmFirst t ->
      (match t with
        TmPair (t1,t2) -> 
          let tyT1 = typeof ctx t1 in
          tyT1
        | _ -> raise (Type_error "argument of first must be a tuple"))

  | TmSecond t ->
      (match t with
        TmPair (t1,t2) -> 
          let tyT1 = typeof ctx t2 in
          tyT1
        | _ -> raise (Type_error "argument of second must be a tuple"))

  | TmRecord t ->
    let rec aux ty t =
      match t with
        (s,tm)::tail -> aux ((typeof ctx tm)::ty) tail
        | _ -> TyRecord (List.rev ty)
    in aux [] t
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmStr s ->
      s
  | TmConcat (t1,t2) ->
      string_of_term t1 ^ string_of_term t2
  | TmPair (t1,t2) ->
    "{" ^ string_of_term t1 ^ "," ^ string_of_term t2 ^ "}"
  | TmFirst t ->
      (match t with
        TmPair (t1,t2) -> 
          string_of_term t1
          | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmSecond t ->
      (match t with
        TmPair (t1,t2) -> 
            string_of_term t2
            | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmRecord t ->
    match t with
      (s,tm)::tail -> string_of_term tm ^ "," ^ string_of_term (TmRecord tail)
      | _ -> ""
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmStr s ->
      [s]
  | TmConcat (t1, t2) -> 
      [string_of_term t1 ^ string_of_term t2]
  | TmPair (t1,t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmFirst t ->
      (match t with
        TmPair (t1,t2) ->
          free_vars t1
        | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmSecond t ->
      (match t with
        TmPair (t1,t2) ->
          free_vars t2
        | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmRecord t ->
    let rec aux fv t =
      match t with
        (s,tm)::tail -> aux ((free_vars tm) @ fv) tail
        | _ -> (List.rev fv)
    in aux [] t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmStr s ->
      TmStr s
  | TmConcat (t1,t2) ->
      TmStr (string_of_term (subst x s t1) ^ string_of_term (subst x s t2))
  | TmPair (t1,t2) ->
      TmPair (subst x s t1, subst x s t2)
  | TmFirst t ->
      (match t with
        TmPair (t1,t2) ->
          subst x s t1
          | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmSecond t ->
      (match t with
        TmPair (t1,t2) ->
          subst x s t2
          | _ -> raise (Type_error "argument of second must be a tuple"))
  | TmRecord t ->
    let rec aux sbs t =
      match t with
        (s1,tm)::tail -> aux ((s1,(subst x s tm))::sbs) tail
        | _ -> TmRecord (List.rev sbs)
    in aux [] t
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

  | TmConcat (t1,t2) ->
      let t1' = eval1 ctx t1 in
      let t2' = eval1 ctx t2 in
      TmStr ( string_of_term t1' ^ string_of_term t2')
  
  | TmPair (t1,t2) ->
      let t1' = eval1 ctx t1 in
      let t2' = eval1 ctx t2 in
      TmPair (t1',t2')
  
  | TmFirst t ->
      (match t with
        TmPair (t1,t2) ->
          let t1' = eval1 ctx t1 in
          t1'
        | _ -> raise (Type_error "argument of second must be a tuple"))

  | TmSecond t ->
      (match t with
        TmPair (t1,t2) ->
          let t2' = eval1 ctx t2 in
          t2'
          | _ -> raise (Type_error "argument of second must be a tuple"))
  
  | TmVar s -> 
      fst (getbinding ctx s)

  | TmRecord t ->
    let rec aux ev t =
      match t with
        (s,tm)::tail -> aux ((s,(eval1 ctx tm))::ev) tail
        | _ -> TmRecord (List.rev ev)
    in aux [] t

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  let rec aux vl = function
    | TmTrue -> 
        TmTrue
    | TmFalse ->
        TmFalse
    | TmIf (t1, t2, t3) ->
        TmIf (aux vl t1,aux vl t2,aux vl t3)
    | TmZero ->
        TmZero
    | TmSucc t ->
        TmSucc (aux vl t)
    | TmPred t ->
        TmPred (aux vl t)
    | TmIsZero t ->
        TmIsZero (aux vl t)
    | TmVar s ->
        if List.mem s vl then TmVar s else (fst (getbinding ctx s))
    | TmAbs (s, t, t1) ->
        TmAbs (s, t, (aux (s::vl) t1))
    | TmApp (t1, t2) ->
        TmApp (aux vl t1,aux vl t2)
    | TmLetIn (s, t1, t2) ->
        TmLetIn (s, aux vl t1, (aux (s::vl) t2))
    | TmFix t ->
        TmFix (aux vl t)
    | TmStr s ->
        TmStr s
    | TmConcat (t1, t2) -> 
        TmStr (string_of_term (aux vl t1) ^ string_of_term (aux vl t2))
    | TmPair (t1,t2) -> 
        TmPair (aux vl t1,aux vl t2)
    | TmFirst t ->
        (match t with
          TmPair (t1,t2) ->
            aux vl t1
          | _ -> raise (Type_error "argument of second must be a tuple"))
    | TmSecond t ->
        (match t with
          TmPair (t1,t2) ->
            aux vl t2
          | _ -> raise (Type_error "argument of second must be a tuple"))
    | TmRecord t ->
      let rec aux2 ac t =
        match t with
          (s,tm)::tail -> aux2 ((s,(aux vl tm))::ac) tail
          | _ -> TmRecord (List.rev ac)
      in aux2 [] t
  in aux [] tm
;;



let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

