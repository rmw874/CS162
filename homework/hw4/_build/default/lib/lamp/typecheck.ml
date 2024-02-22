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
  (* arithmetic *)
  | Num of int
  | Binop of binop * expr * expr
  (* binding *)
  | Var of string
  | Scope of string * expr
  (* lambda calculus *)
  | Lambda of ty option * expr
  | App of expr * expr
  (* let expression *)
  | Let of expr * expr
  (* booleans *)
  | True
  | False
  | IfThenElse of expr * expr * expr
  | Comp of relop * expr * expr
  (* lists *)
  | ListNil of ty option
  | ListCons of expr * expr
  | ListMatch of expr * expr * expr
  (* fix *)
  | Fix of ty option * expr
  (* products *)
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  (* type annotation *)
  | Annot of expr * ty   
*)
let rec abstract_eval (gamma : gamma) (e : expr) : ty =
  try
    match e with
    (* arithmetic *)
    | Num _ -> TInt
    | Binop (_, e1, e2) -> (match (abstract_eval gamma e1, abstract_eval gamma e2) with
                            | (TInt, TInt) -> TInt (** if both operands are integers, then the result is an integer *)
                            | _ -> ty_err ("[synth] ill-formed binary operation" ^ show_expr e))

    (* binding *)
    | Var x -> (match find gamma x with
                | Some t -> t (** if the variable is in the environment, then return its type *)
                | None -> ty_err ("[synth] undefined variable: " ^ x))
    | Scope (x, e) -> abstract_eval (add gamma x (abstract_eval gamma e)) e (** add the type and the variable to the environment and evaluate the expression *)

    (* lambda calculus *)
    | Lambda (t, e) -> (match t with
                        | Some t1 -> TFun (t1, abstract_eval gamma e) (** if the type is given, then the type of the lambda is the function type *)
                        | None -> ty_err ("[synth] ill-formed lambda" ^ show_expr e))
    | App (e1, e2) -> (match (abstract_eval gamma e1, abstract_eval gamma e2) with
                      | (TFun (t1, t2), t3) -> (match equal_ty t1 t3 with
                                                | true -> t2 (** if the type of the function is the same as the type of the argument, then the type of the application is the type of the output *)
                                                | false -> ty_err ("[synth] ill-formed application" ^ show_expr e))
                      | _ -> ty_err ("[synth] ill-formed application" ^ show_expr e))

    (* let expression *)
    | Let (e1, e2) -> abstract_eval (add gamma "v" (abstract_eval gamma e1)) e2
    | True -> TBool
    | False -> TBool
    | IfThenElse (e1, e2, e3) -> (match (abstract_eval gamma e1, abstract_eval gamma e2, abstract_eval gamma e3) with
                                  | (TBool, t2, t3) -> (match equal_ty t2 t3 with
                                                        | true -> t2 (** if the condition is a boolean, then the types of the branches must be the same (t1 = t2) *)
                                                        | false -> ty_err ("[synth] ill-formed if-then-else" ^ show_expr e))
                                  | _ -> ty_err ("[synth] ill-formed if-then-else" ^ show_expr e))
    | Comp (_, e1, e2) -> (match (abstract_eval gamma e1, abstract_eval gamma e2) with
                          | (TInt, TInt) -> TBool (** if both operands are integers, then the result is a boolean *)
                          | _ -> ty_err ("[synth] ill-formed comparison" ^ show_expr e))

    (* lists *)
    | ListNil (t) -> (match t with
                      | Some t1 -> TList t1 (** if the type is given, then the type of the list is the type of the elements *)
                      | None -> ty_err ("[synth] ill-formed listnil" ^ show_expr e))
    | ListCons (e1, e2) -> (match abstract_eval gamma e1 with
                            | t1 -> (match abstract_eval gamma e2 with
                                     | TList t2 -> (match equal_ty t1 t2 with
                                                    | true -> TList t1
                                                    | false -> ty_err ("[synth] ill-formed list cons" ^ show_expr e2))
                                     | _ -> ty_err ("[synth] ill-formed list cons" ^ show_expr e2)))
    | ListMatch (e1, e2, e3) -> (match abstract_eval gamma e1 with
                                  | TList _ -> let t2 = abstract_eval gamma e2 in
                                               let t3 = abstract_eval gamma e3 in
                                               (match equal_ty t2 t3 with
                                                | true -> t2
                                                | false -> ty_err ("[synth] ill-formed listmatch" ^ show_expr e3))
                                  | _ -> ty_err ("[synth] ill-formed listmatch" ^ show_expr e1))

    (* fix *)
    | Fix (t, e) -> (match t with
                     | Some t1 -> (match equal_ty t1 (abstract_eval gamma e) with 
                                   | true -> t1 (** if the type is given and the type of the expression is the same, then the type of the fix is the given type *)
                                   | false -> ty_err ("[synth] ill-formed fix" ^ show_expr e))
                     | None -> ty_err ("[synth] ill-formed fix" ^ show_expr e)) (** if the type is not given, then the type of the fix is the type of the expression *)

    (* products *)
    | Pair (e1, e2) -> TProd (abstract_eval gamma e1, abstract_eval gamma e2)
    | Fst e -> (match abstract_eval gamma e with
                | TProd (t1, _) -> t1 (** the type of the first projection is the type of the first element of the pair *)
                | _ -> ty_err ("[synth] ill-formed fst" ^ show_expr e))
    | Snd e -> (match abstract_eval gamma e with
                | TProd (_, t2) -> t2 (** the type of the second projection is the type of the second element of the pair *)
                | _ -> ty_err ("[synth] ill-formed snd projection" ^ show_expr e))

    (* type annotation *)
    | Annot (e, t) -> (match abstract_eval gamma e with
                      | t1 -> (match equal_ty t t1 with
                               | true -> t
                               | false -> ty_err ("[synth] ill-formed annotation" ^ show_expr e)) (** if the type of the expression is the same as the given type, then the type of the expression is the given type *)
                      )
    | _ -> ty_err ("[synth] ill-formed: " ^ show_expr e)
  with Type_error msg -> ty_err (msg ^ "\nin expression " ^ show_expr e)
