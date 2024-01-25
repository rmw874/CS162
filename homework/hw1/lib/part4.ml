open Base
open Util

type expr =
  | Const of int
  | X
  | Add of expr * expr
  | Mul of expr * expr
  | Compose of expr * expr

(* Pretty-printer *)
let rec pp_expr ppf = function
  | Const n -> Fmt.pf ppf "%d" n
  | X -> Fmt.pf ppf "x"
  | Add (e1, e2) -> Fmt.pf ppf "(%a + %a)" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> Fmt.pf ppf "(%a * %a)" pp_expr e1 pp_expr e2
  | Compose (e1, e2) -> Fmt.pf ppf "(%a; %a)" pp_expr e1 pp_expr e2

(* Convert an expression into a pretty string *)
let show_expr (e : expr) : string = Fmt.to_to_string pp_expr e
let rec eval_expr (x : int) (e : expr) : int = 
  match e with
  | Const n -> n
  | X -> x
  | Add (e1, e2) -> (eval_expr x e1) + (* evaluate e1, then add it to evaluated e2 *)
                    (eval_expr x e2)   
  | Mul (e1, e2) -> (eval_expr x e1) * 
                    (eval_expr x e2)
  | Compose (e1, e2) -> eval_expr (eval_expr x e1) e2 (* evaluate e1, then insert it into e2 *)

let rec simplify (e : expr) : expr = 
  match e with
  | Const n -> Const n
  | X -> X
  | Add (e1, e2) -> 
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match (e1', e2') with
    | (Const n1, Const n2) -> Const (n1 + n2) (* if both are consts, add them *)
    | (Const 0, e) -> e (* 0 + num is just num *)
    | (e, Const 0) -> e (* num + 0 is just num *)
    | _ -> Add (e1', e2')) (* otherwise, just add *)
  | Mul (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match (e1', e2') with
    | (Const n1, Const n2) -> Const (n1 * n2) (* if both are consts, multiply them *)
    | (Const 0, _) -> Const 0 (* 0 * num is 0 *)
    | (_, Const 0) -> Const 0 (* num * 0 is 0 *)
    | (Const 1, e) -> e (* 1 * num is num *)
    | (e, Const 1) -> e (* num * 1 is num *)
    | _ -> Mul (e1', e2')) (* otherwise, just multiply *)
  | Compose (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match (e1', e2') with
    | (Const n1, Const n2) -> Const (n1 * n2) 
    | (Const 0, _) -> Const 0 
    | (_, Const 0) -> Const 0 
    | (Const 1, e) -> e 
    | (e, Const 1) -> e 
    | _ -> Compose (e1', e2')) 


type poly = int list [@@deriving show]

let rec eval_poly (x : int) (p : poly) : int = 
  match p with
  | [] -> 0
  | h::t -> h + x * (eval_poly x t)

let rec normalize (e : expr) : poly = bonus ()
let semantic_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
