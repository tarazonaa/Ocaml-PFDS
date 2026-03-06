module Make (Ord : Set.OrderedType) = struct
  type elt = Ord.t

  type tree = Empty | Node of tree * elt * tree

  let empty = Empty

  let rec bigger pivot = function
    | Empty ->
        Empty
    | Node (a, x, b) -> (
        if Ord.compare x pivot <= 0 then bigger pivot b
        else
          match a with
          | Empty ->
              Node (Empty, x, b)
          | Node (a', y, a'') ->
              if Ord.compare y pivot <= 0 then Node (bigger pivot a'', x, b)
              else Node (bigger pivot a', y, Node (a'', x, b)) )

  let rec smaller pivot = function
    | Empty ->
        Empty
    | Node (a, x, b) ->
        if Ord.compare x pivot > 0 then
          match a with
          | Empty ->
              Empty
          | Node (a', y, a'') ->
              if Ord.compare y pivot > 0 then smaller pivot a'
              else Node (smaller pivot a'', x, b)
        else Node (a, x, smaller pivot b)

  let rec partition pivot = function
    | Empty ->
        (Empty, Empty)
    | Node (a, x, b) as t -> (
        if Ord.compare x pivot <= 0 then
          match b with
          | Empty ->
              (t, Empty)
          | Node (b1, y, b2) ->
              if Ord.compare y pivot <= 0 then
                let small, big = partition pivot b2 in
                (Node (Node (a, x, b1), y, small), big)
              else
                let small, big = partition pivot b1 in
                (Node (a, x, small), Node (big, y, b2))
        else
          match a with
          | Empty ->
              (Empty, t)
          | Node (a1, y, a2) ->
              if Ord.compare y pivot <= 0 then
                let small, big = partition pivot a2 in
                (Node (a1, y, small), Node (big, x, b))
              else
                let small, big = partition pivot a1 in
                (small, Node (big, y, Node (a2, x, b))) )

  let rec findMin = function
    | Empty ->
        assert false
    | Node (Empty, x, b) ->
        x
    | Node (a, x, b) ->
        findMin a

  let rec deleteMin = function
    | Empty ->
        assert false
    | Node (Empty, x, b) ->
        b
    | Node (Node (Empty, x, b), y, c) ->
        Node (b, y, c)
    | Node (Node (a, x, b), y, c) ->
        Node (deleteMin a, x, Node (b, y, c))

  let insert x tree =
    let small, big = partition x tree in
    Node (small, x, big)
end
