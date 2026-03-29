module Make (Ord : Set.OrderedType) = struct
  type color = Red | Black

  type elt = Ord.t * bool

  type tree = Empty | Node of color * tree * elt * tree

  type t = {tree: tree; size: int; deleted: int}

  let empty = {tree= Empty; size= 0; deleted= 0}

  let rec member x = function
    | Empty ->
        Error "Empty node"
    | Node (_, a, y, b) ->
        if Ord.compare x (fst y) < 0 then member x a
        else if Ord.compare x (fst y) > 0 then member x b
        else Ok true

  let balance = function
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
        Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | color, a, x, b ->
        Node (color, a, x, b)

  let insert x s =
    let rec ins = function
      | Empty ->
          Node (Red, Empty, x, Empty)
      | Node (color, a, y, b) as node ->
          if Ord.compare (fst x) (fst y) < 0 then balance (color, ins a, y, b)
          else if Ord.compare (fst x) (fst y) > 0 then
            balance (color, a, y, ins b)
          else node
    in
    match ins s.tree with
    | Empty ->
        Error "Cannot insert"
    | Node (_, a, y, b) ->
        Ok {s with tree= Node (Black, a, y, b); size= s.size + 1}

  let rebuild s =
    let rec to_list = function
      | Empty ->
          []
      | Node (_, a, (v, deleted), b) ->
          to_list a @ (if deleted then [] else [(v, false)]) @ to_list b
    in
    List.fold_left
      (fun acc x -> match insert x acc with Ok t -> t | Error _ -> acc)
      empty (to_list s.tree)

  let delete x s =
    let rec del = function
      | Empty ->
          Empty
      | Node (color, a, y, b) ->
          if Ord.compare (fst y) x == 0 then Node (color, a, (fst y, true), b)
          else if Ord.compare (fst y) x > 0 then Node (color, del a, y, b)
          else Node (color, a, y, del b)
    in
    match del s.tree with
    | Empty ->
        Error "Cannot delete from empty tree"
    | Node (_, a, y, b) ->
        let s' = {s with tree= Node (Black, a, y, b); deleted= s.deleted + 1} in
        if s'.deleted > s'.size / 2 then Ok (rebuild s') else s'
end
