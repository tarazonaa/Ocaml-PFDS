type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let rec insert node = function
  | Empty ->
      Node (Empty, node, Empty)
  | Node (left, value, right) ->
      if node > value then Node (left, value, insert node right)
      else if node < value then Node (insert node left, value, right)
      else Node (left, value, right)

module Leftist (Ord : Set.OrderedType) = struct
  type leftist = Empty | Node of int * Ord.t * leftist * leftist

  let rank = function Empty -> 0 | Node (r, _, _, _) -> r

  let makeT (x, a, b) =
    if rank a >= rank b then Node (rank b + 1, x, a, b)
    else Node (rank a + 1, x, b, a)

  let empty () = Empty

  let isEmpty = function Empty -> true | Node _ -> false

  let rec merge leftist1 leftist2 =
    match leftist1 with
    | Empty ->
        leftist2
    | Node (_, x, a1, b1) -> (
      match leftist2 with
      | Empty ->
          leftist1
      | Node (_, y, a2, b2) ->
          if Ord.compare x y <= 0 then makeT (x, a1, merge b1 leftist2)
          else makeT (y, a2, merge leftist2 b1) )

  let insert x h = merge (Node (1, x, Empty, Empty)) h

  let findMin = function
    | Empty ->
        failwith "Cannot find minimum with empty tree"
    | Node (_, x, _, _) ->
        x

  let deleteMin = function
    | Empty ->
        failwith "Cannot delete minimum from empty tree."
    | Node (_, x, a, b) ->
        merge a b
end
