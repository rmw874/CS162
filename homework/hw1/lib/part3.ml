open Base
open Util

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree [@@deriving show]

let rec equal_tree (equal : 'a -> 'a -> bool) (t1 : 'a tree) (t2 : 'a tree) :
    bool =
  match (t1, t2) with
  | Leaf, Leaf -> true (*if the trees are both leaves, they are equal*)
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
      equal x1 x2 && equal_tree equal l1 l2 && equal_tree equal r1 r2 (*if the trees are both nodes, they are equal if the values are equal and the left and right subtrees are equal*)
  | _ -> false (*if the trees are not both leaves or both nodes, they are not equal, and we can return false*)

let rec dfs_helper current_timestamp = function
  | Leaf -> (Leaf, current_timestamp)
  | Node (value, l, r) ->
      (* label the current node first, then proceed to the left and right subtrees *)
      let (label_l, next_timestamp) = dfs_helper (current_timestamp + 1) l in (*the next timestamp is the current timestamp plus one*)
      let (label_r, final_timestamp) = dfs_helper next_timestamp r in (*the final timestamp is the next timestamp plus one*)
      (Node ((current_timestamp, value), label_l, label_r), final_timestamp) (*return the labeled tree and the final timestamp*)
let timestamp (t : 'a tree) : (int * 'a) tree =
  let (labeled_tree, _) = dfs_helper 0 t in (*start the timestamp at 0*)
  labeled_tree