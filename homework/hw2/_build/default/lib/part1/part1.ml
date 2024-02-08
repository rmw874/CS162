open Base
open Util

let todo () = failwith "TODO"
let singletons (xs : 'a list) : 'a list list = 
  (* using map, for each element in xs, we create a list containing only that element. this is repeated for all elements in xs *)
  map (fun x -> [x]) xs

let map2d (f : 'a -> 'b) (xss : 'a list list) : 'b list list = 
  (* for each list in xss, we use map to apply f to each element in the list. Then we use map to do this for each list in xss until we have a 2d list of the results *)
  map (fun xs -> map f xs) xss

let product (xs : 'a list) (ys : 'b list) : ('a * 'b) list list =
  (* for each x, we use map to create a list of (x, y) pairs for each y in ys. Then we use map to do this for each x in xs until we have a list of lists of (x, y) pairs *)
  map (fun x -> map (fun y -> (x, y)) ys) xs 

let rec solve (base_case : 'result) (combine : 'a -> 'result -> 'result)
    (xs : 'a list) : 'result =
  match xs with
  | [] -> base_case
  | x :: xs' ->
      let r = solve base_case combine xs' in
      combine x r

let power (xs : 'a list) : 'a list list = 
  (* using solve, which is the same as fold_right, we can build the power set of a list by starting with [[]] and then adding each element to each list in the accumulator *) 
  solve [[]] (fun x acc -> 
    acc @ map (fun ys -> x :: ys) acc) xs
let join2 : 'a option -> ('b option -> ('a * 'b) option) =
    fun x ->
       match x with
       | Some x -> fun y -> (match y with (* doing it this way, we can incoorporate the None case into the match without having to use a separate function *)
                             | Some y -> Some (x, y)
                             | None -> None)
        (* if x is None, then the result is None regardless of y *)
       | None -> fun _ -> None 