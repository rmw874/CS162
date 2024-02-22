open Ast
open Base

type gamma = (string * ty) list

(** Find the type of a variable in gamma *)
let find : gamma -> string -> ty option = List.Assoc.find ~equal:String.equal

(** Add a (var, type) pair to gamma *)
let add : gamma -> string -> ty -> gamma = List.Assoc.add ~equal:String.equal

exception Type_error of string

let ty_err msg = raise (Type_error msg)
let todo () = failwith "TODO"

(**type ty = TInt | TBool | TFun of ty * ty | TList of ty | TProd of ty * ty*)
let rec equal_ty (t1 : ty) (t2 : ty) : bool = match (t1, t2) with
                                              | (TInt, TInt) -> true
                                              | (TBool, TBool) -> true
                                              | (TFun (t1, t2), TFun (t3, t4)) -> equal_ty t1 t3 && equal_ty t2 t4 (** if the input and output types are the same, then the functions are the same *)
                                              | (TList t1, TList t2) -> equal_ty t1 t2 (** if the types of the elements are the same, then the lists are the same *)
                                              | (TProd (t1, t2), TProd (t3, t4)) -> equal_ty t1 t3 && equal_ty t2 t4 (** same as TFun *)
                                              | _ -> false

(** Synthesize the type of expression e under the typing environment gamma *)

(** Abstractly evaluate expression e to some type
  * under the typing environment gamma *)

(**
type expr =
  | Num of int
  | Binop of binop * expr * expr
  | Var of string
  | Scope of string * expr
  | Lambda of ty option * expr
  | App of expr * expr
  | Let of expr * expr
  | True
  | False
  | IfThenElse of expr * expr * expr
  | Comp of relop * expr * expr
  | ListNil of ty option
  | ListCons of expr * expr
  | ListMatch of expr * expr * expr
  | Fix of ty option * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Annot of expr * ty    
*)
let rec abstract_eval (gamma : gamma) (e : expr) : ty =
  try
    match e with
    (**Num of int*)
    | Num _ -> TInt
    (**Binop of binop * expr * expr*)
    | Binop (_, e1, e2) -> (match (abstract_eval gamma e1, abstract_eval gamma e2) with
                            | (TInt, TInt) -> TInt (** if both operands are integers, then the result is an integer *)
                            | _ -> ty_err ("[synth] ill-formed binary operation"))
    (**Var of string*)
    | Var x -> (match find gamma x with
                | Some t -> t (** if the variable is in the environment, then return its type *)
                | None -> ty_err ("[synth] undefined variable: " ^ x))
    (**Scope of string * expr*)
    | Scope (x, e) -> abstract_eval (add gamma x (abstract_eval gamma e)) e (** add the type and the variable to the environment and evaluate the expression *)
    (**Lambda of ty option * expr*)
    | Lambda (t, e) -> (match t with
                        | Some t1 -> TFun (t1, abstract_eval gamma e) (** if the type is given, then the type of the lambda is the function type *)
                        | None -> ty_err ("[synth] ill-formed lambda"))
    (**Let of expr * expr*)
    | Let (e1, e2) -> abstract_eval (add gamma "v" (abstract_eval gamma e1)) e2
    (**App of expr * expr*)
    | App (e1, e2) -> todo()
    (**True*)
    | True -> TBool
    (**False*)
    | False -> TBool
    (**IfThenElse of expr * expr * expr*)
    | IfThenElse (e1, e2, e3) -> (match (abstract_eval gamma e1, abstract_eval gamma e2, abstract_eval gamma e3) with
                                  | (TBool, t2, t3) -> if equal_ty t2 t3 then t2 else ty_err ("[synth] ill-formed if-then-else") (** if the condition is a boolean, then the types of the branches must be the same *)
                                  | _ -> ty_err ("[synth] ill-formed if-then-else"))
    (**Comp of relop * expr * expr*)
    | Comp (_, e1, e2) -> (match (abstract_eval gamma e1, abstract_eval gamma e2) with
                          | (TInt, TInt) -> TBool (** if both operands are integers, then the result is a boolean *)
                          | _ -> ty_err ("[synth] ill-formed comparison"))
    (**ListNil of ty option*)
    | ListNil (t) -> (match t with
                      | Some t1 -> TList t1 (** if the type is given, then the type of the list is the type of the elements *)
                      | None -> ty_err ("[synth] ill-formed list nil"))
    | ListCons (e1, e2) -> (match abstract_eval gamma e1 with
                            | t1 -> (match abstract_eval gamma e2 with
                                     | TList t2 -> if equal_ty t1 t2 then TList t1 else ty_err ("[synth] ill-formed list cons") (** if the types of the elements are the same, then the type of the list is the type of the elements *)
                                     | _ -> ty_err ("[synth] ill-formed list cons"))
                            | _ -> ty_err ("[synth] ill-formed list cons"))
    (**ListMatch of expr * expr * expr*)
    | ListMatch (e1, e2, e3) -> (match abstract_eval gamma e1 with
                                  | TList _ -> let t2 = abstract_eval gamma e2 in
                                               let t3 = abstract_eval gamma e3 in
                                               if equal_ty t2 t3 then t2 else ty_err ("[synth] ill-formed list match")
                                  | _ -> ty_err ("[synth] ill-formed list match"))
    (**Fix of ty option * expr*)
    | Fix (t, e) -> (match t with
                     | Some t1 -> if equal_ty t1 (abstract_eval gamma e) then t1 else ty_err ("[synth] ill-formed fix") (** if the type is given, then the type of the fix is the same as the given type *)
                     | None -> ty_err ("[synth] ill-formed fix"))
    (**Pair of expr * expr*)
    | Pair (e1, e2) -> TProd (abstract_eval gamma e1, abstract_eval gamma e2)
    (**Fst of expr*)
    | Fst e -> (match abstract_eval gamma e with
                | TProd (t1, _) -> t1 (** the type of the first projection is the type of the first element of the pair *)
                | _ -> ty_err ("[synth] ill-formed first projection"))
    (**Snd of expr*)
    | Snd e -> (match abstract_eval gamma e with
                | TProd (_, t2) -> t2 (** the type of the second projection is the type of the second element of the pair *)
                | _ -> ty_err ("[synth] ill-formed second projection"))
    (**Annot of expr * ty*)
    | Annot (e, t) -> todo()
    | _ -> ty_err ("[synth] ill-formed: " ^ show_expr e)
  with Type_error msg -> ty_err (msg ^ "\nin expression " ^ show_expr e)
