module Make (Ord : Set.OrderedType) = struct
  type color = Red | Black

  type tree = Empty | Node of color * tree * Ord.t * tree

  let empty () = Empty

  let rec member x = function
    | Empty ->
        Error "Empty node"
    | Node (_, a, y, b) ->
        if Ord.compare x y < 0 then member x a
        else if Ord.compare x y > 0 then member x b
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
          if Ord.compare x y < 0 then balance (color, ins a, y, b)
          else if Ord.compare x y > 0 then balance (color, a, y, ins b)
          else node
    in
    match ins s with
    | Empty ->
        Error "Cannot insert"
    | Node (_, a, y, b) ->
        Ok (Node (Black, a, y, b))
end
