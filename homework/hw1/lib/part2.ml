open Base
open Util

let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list =
  match xs with
  | [] -> [] (* if the list is empty, then no way jose *)
  | [ x ] -> [ x ] (* if there is only one element, then we can't compress it *)
  | x :: y :: ys -> if equal x y then compress equal (y :: ys) (* if x = y, then we can just remove x *)
                    else x :: compress equal (y :: ys) (* if x != y, then we need to keep x and compress the rest *)

  let max (xs : int list) : int option =
    let rec max_finder curr_max = function
      | [] -> curr_max
      | x :: xs' -> max_finder (if x > curr_max then x (* if x is greater than curr_max, x is the new curr_max *)
                                else curr_max) xs' (* else curr_max is still the curr_max, and we keep checking *)
    in
    match xs with
    | [] -> None 
    | x :: xs' -> Some (max_finder x xs') (* we use the helper function *)

let rec join (xs : 'a option list) : 'a list option =
  match xs with
  | [] -> Some []
  | None :: _ -> None (* if there is a None in the list, then the whole thing is None *)
  | Some x :: xs -> 
    match join xs with 
    | None -> None (* if there is a None in the rest of the list, then the whole thing is None *)
    | Some xs -> Some (x :: xs) (* if there is no None in the rest of the list, then we can just add x to the rest of the list *)
let insert (k : 'k) (v : 'v) (d : ('k * 'v) list) : ('k * 'v) list = (k, v) :: d

let rec lookup (equal : 'k -> 'k -> bool) (k : 'k) (d : ('k * 'v) list) :
    'v option =
      match d with 
      | [] -> None
      | (k', v) :: d' -> if equal k k' then Some v (* if k = k', then we found the value *)
                         else lookup equal k d' (* else we keep looking *)
