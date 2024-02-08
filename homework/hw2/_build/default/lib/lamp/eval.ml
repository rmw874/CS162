open Base
open Ast

let todo () = failwith "TODO"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  (***Problem 4** (🧑‍💻, 5 points): Implement the free variable function `free_vars: expr -> Vars.t` that computes the set of free variable references in an expression. The `Vars` module provides a type (`Vars.t`) to represent a set of strings, and functions that operate on such sets.*)
  match e with
  | Num _ -> empty
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> singleton x
  | Scope (x, e) -> diff (free_vars e) (singleton x)
  | Lambda e -> free_vars e
  | App (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Let (e1, e2) -> union (free_vars e1) (free_vars e2)

(** Perform substitution c[x -> e], i.e., substituting x with e in c *Hints*: 
1. You may find it helpful to define helper functions that are similar to you did with list-based dictionaries in HW1. It's just that this time, we only need to keep track of the keys, not the values, so we use `string list` instead of `(string * 'v) list`.
2. Each todo can be implemented with just 1 line of code, although that 1 line may be a call to a helper function. If you wrote more than 1 line, you are probably overthinking it.
*)

let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with
  | Num n -> Num n
  | Binop (op, c1, c2) -> Binop (op, subst x e c1, subst x e c2)
  | Var y -> if String.equal x y then e else Var y
  | Scope (y, c') -> if String.equal x y then Scope (y, c') else Scope (y, subst x e c') (* capture-avoiding substitution *)
  | Lambda c' -> Lambda (subst x e c')
  | App (c1, c2) -> App (subst x e c1, subst x e c2)
  | Let (c1, c2) -> Let (subst x e c1, subst x e c2)

(** Evaluate expression e *)
(*
The `eval` function will "run" your program by performing binary operations, evaluating function arguments, 
applying arguments to functions using `subst`, etc. 
You might want to refer to Section 4 (Operational Semantics) of the [language reference manual](../lamp.pdf) 
for the precise meaning of each language construct. 
If no evaluation rule applies, then your interpreter should call `im_stuck` to signal that the interpreter is stuck.   
*)

(*type binop = Add | Sub | Mul*)

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

    | Let (e1, Scope (x, e2)) -> (match eval e1 with
      | e1' -> eval (subst x e1' e2)
      | _ -> im_stuck (Fmt.str "Cannot evaluate let: %a" Pretty.pp_expr e)
    )

    | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.pp_expr e)
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.pp_expr e)
