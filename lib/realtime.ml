module Make (Ord : Set.OrderedType) : Queue.S with type elt = Ord.t = struct
  module S = Streams.Make (Ord)

  type elt = Ord.t

  type t = S.stream * elt list * S.stream

  let empty = (lazy S.Nil, [], lazy S.Nil)

  let isEmpty (f, r, s) =
    match Lazy.force f with S.Nil -> true | S.Cons _ -> false

  let rec rotate f r a =
    lazy
      ( match (Lazy.force f, r) with
      | S.Nil, y :: _ ->
          S.Cons (y, a)
      | S.Cons (x, xs), y :: ys ->
          S.Cons (x, rotate xs ys (lazy (S.Cons (y, a))))
      | _ ->
          assert false )

  let exec (f, r, s) =
    match Lazy.force s with
    | S.Nil ->
        let f' = rotate f r (lazy S.Nil) in
        (f', [], f')
    | S.Cons (x, y) ->
        (f, r, y)

  let snoc x (f, r, s) = exec (f, x :: r, s)

  let head (f, r, s) =
    match Lazy.force f with S.Nil -> assert false | S.Cons (x, y) -> x

  let tail (f, r, s) =
    match Lazy.force f with
    | S.Nil ->
        assert false
    | S.Cons (x, y) ->
        exec (y, r, s)
end
