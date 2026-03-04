type rank = int

type 'a tree = Node of rank * 'a * 'a tree list

let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
  if x1 <= x2 then Node (r + 1, x1, t2 :: c1) else Node (r + 1, x2, t1 :: c2)

let rank (Node (r, x, c)) = r

let root (Node (r, x, c)) = x

type 'a heap = 'a tree list

let rec insTree t = function
  | [] ->
      [t]
  | x :: xs as ts ->
      if rank t < rank x then t :: ts else insTree (link t x) xs

let insert x ts = insTree (Node (0, x, [])) ts

let rec merge h1 h2 =
  match h1 with
  | [] ->
      failwith "Cannot merge empty heap"
  | x :: xs -> (
    match h2 with
    | [] ->
        failwith "Cannot merge empty heap"
    | y :: ys ->
        if rank x < rank y then x :: merge xs h2
        else if rank y < rank x then y :: merge h1 ys
        else insTree (link x y) (merge xs ys) )

let rec removeMinTree = function
  | [] ->
      failwith "Cannot remove min from empty heap"
  | [t] ->
      (t, [])
  | x :: xs ->
      let t, ts = removeMinTree xs in
      if root x <= root t then (x, xs) else (t, x :: ts)

let findMin ts =
  let t, _ = removeMinTree ts in
  root t

let deleteMin ts =
  let Node (_, x, c), tail = removeMinTree ts in
  merge (List.rev c) tail
