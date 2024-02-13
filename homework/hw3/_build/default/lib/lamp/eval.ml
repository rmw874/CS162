open Base
open Ast

let todo () = Num (-1000)
let hmm () = Num (-2000)

(** Exception indicating that evaluation is stuck *)

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  (* Your code goes here *)
  match e with
  | Num _ -> empty
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> singleton x
  | Scope (x, e') -> diff (free_vars e') (singleton x) (*find every free variable in e'. cannot be x, as x is bounded in this scope*)
  | Lambda e -> free_vars e
  | App (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Let (e1, e2) -> union (free_vars e1) (free_vars e2)
  | True | False -> empty
  | IfThenElse (e1, e2, e3) -> union (union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Comp (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | ListNil -> empty
  | ListCons (e1, e2) -> union (free_vars e1) (free_vars e2)
  | ListMatch (e1, e2, e3) -> union (union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fix e' -> free_vars e'
  | Pair (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Fst e' -> free_vars e'
  | Snd e' -> free_vars e'

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with
  | Num n -> Num n
  | Binop (op, c1, c2) -> Binop (op, subst x e c1, subst x e c2)
  | Var y -> if String.equal x y then e else Var y
  | Scope (y, c') -> if String.equal x y then Scope (y, c') else Scope (y, subst x e c') (* capture-avoiding substitution *)
  | Lambda c' -> Lambda (subst x e c')
  | App (c1, c2) -> App (subst x e c1, subst x e c2)
  | Let (c1, c2) -> Let (subst x e c1, subst x e c2)
  | True | False -> c
  | IfThenElse (c1, c2, c3) -> IfThenElse (subst x e c1, subst x e c2, subst x e c3)
  | Comp (op, c1, c2) -> Comp (op, subst x e c1, subst x e c2)
  | ListNil -> c
  | ListCons (c1, c2) -> ListCons (subst x e c1, subst x e c2)
  | ListMatch (c1, c2, c3) -> ListMatch (subst x e c1, subst x e c2, subst x e c3)
  | Fix c' -> Fix (subst x e c')
  | Pair (c1, c2) -> Pair (subst x e c1, subst x e c2)
  | Fst c' -> Fst (subst x e c')
  | Snd c' -> Snd (subst x e c')

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    | Num n -> Num n
    | Binop (op, e1, e2) -> (match op with
      | Add -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> Num (n1 + n2)
        | _ -> im_stuck (Fmt.str "Cannot add values: %a" Pretty.pp_expr e))
      | Sub -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> Num (n1 - n2)
        | _ -> im_stuck (Fmt.str "Cannot subtract values: %a" Pretty.pp_expr e))
      | Mul -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> Num (n1 * n2)
        | _ -> im_stuck (Fmt.str "Cannot multiply: %a" Pretty.pp_expr e)))

    | Var _ -> im_stuck (Fmt.str "Variable cannot be evaluated alone: %a" Pretty.pp_expr e)

    | Scope _ ->
        im_stuck (Fmt.str "Scope cannot be evaluated alone: %a" Pretty.pp_expr e)

    | Lambda _ -> e (* just run e *)

    | App (e1, e2) -> let e1' = eval e1 in
        let e2' = eval e2 in
        (match e1' with
        | Lambda (Scope (x, body)) -> eval (subst x e2' body)
        | _ -> im_stuck "Application of non-lambda")

    | Let (e1, Scope (x, e2)) -> eval (subst x (eval e1) e2)

    | True -> True

    | False -> False 

    | IfThenElse (e1, e2, e3) -> (match eval e1 with
      | True -> eval e2
      | False -> eval e3
      | _ -> im_stuck (Fmt.str "Condition is not a boolean: %a" Pretty.pp_expr e))

    | Comp (op, e1, e2) -> (match op with (* comp does the same as binop, but with comparison operators *)
      | Eq -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> if n1 = n2 then True 
                                         else False
        | _ -> im_stuck (Fmt.str "Cannot compare values: %a" Pretty.pp_expr e))
      | Lt -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> if n1 < n2 then True
                                         else False
        | _ -> im_stuck (Fmt.str "Cannot compare values: %a" Pretty.pp_expr e))
      | Gt -> (match (eval e1, eval e2) with
        | (Num n1, Num n2) -> if n1 > n2 then True
                                         else False
        | _ -> im_stuck (Fmt.str "Cannot compare values: %a" Pretty.pp_expr e))
      )

    | ListNil -> ListNil
    
    | ListCons (e1, e2) -> ListCons (eval e1, eval e2)
    
    | ListMatch (e1, e2, Scope (x, Scope (y, e3))) -> 
      (match eval e1 with
      | ListNil -> eval e2
      | ListCons (h, t) -> eval (subst x h (subst y t e3))
      | _ -> im_stuck (Fmt.str "Cannot match list: %a" Pretty.pp_expr e)
    )

    | Fix (Scope (x, e')) -> eval (subst x (Fix (Scope (x, e'))) e')

    | Pair (e1, e2) -> Pair (eval e1, eval e2)
    
    | Fst e' -> (match eval e' with
      | Pair (e1, _) -> eval e1
      | _ -> im_stuck (Fmt.str "Cannot get first element of non-pair: %a" Pretty.pp_expr e)
    )
    
    | Snd e' -> (match eval e' with
    | Pair (_, e2) -> eval e2
    | _ -> im_stuck (Fmt.str "Cannot get second element of non-pair: %a" Pretty.pp_expr e)
  )
    
    | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.pp_expr e)
  
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.pp_expr e)

let eval_fast (e : expr) = failwith ".."
