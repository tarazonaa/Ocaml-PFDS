module Make (Ord : Set.OrderedType) = struct
  type rank = int

  type tree = Node of rank * Ord.t * tree list

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Ord.compare x1 x2 <= 0 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rank (Node (r, _, _)) = r

  let root (Node (_, x, _)) = x

  type heap = tree list

  let rec insTree t = function
    | [] ->
        [t]
    | x :: xs as ts ->
        if rank t < rank x then t :: ts else insTree (link t x) xs

  let insert x ts = insTree (Node (0, x, [])) ts

  let rec merge h1 h2 =
    match (h1, h2) with
    | [], h2 ->
        h2
    | h1, [] ->
        h1
    | x :: xs, y :: ys ->
        if rank x < rank y then x :: merge xs h2
        else if rank y < rank x then y :: merge h1 ys
        else insTree (link x y) (merge xs ys)

  let rec removeMinTree = function
    | [] ->
        failwith "Cannot remove min from empty heap"
    | [t] ->
        (t, [])
    | x :: xs ->
        let t, ts = removeMinTree xs in
        if Ord.compare (root x) (root t) <= 0 then (x, xs) else (t, x :: ts)

  let findMin = function
    | [] ->
        Error "empty heap"
    | ts ->
        let t, _ = removeMinTree ts in
        Ok (root t)

  let deleteMin = function
    | [] ->
        Error "empty heap"
    | ts ->
        let Node (_, x, c), tail = removeMinTree ts in
        Ok (merge (List.rev c) tail)
end
