module Make (Ord : Set.OrderedType) : Queue.S with type elt = Ord.t = struct
  module S = Streams.Make (Ord)

  type elt = Ord.t

  type t = int * S.stream * int * S.stream

  let empty = (0, lazy S.Nil, 0, lazy S.Nil)

  let size (lenf, f, lenr, r) = lenf + lenr

  let isEmpty (lenf, _, _, _) = lenf = 0

  let check (lenf, f, lenr, r) =
    if lenr > lenf then (lenf + lenr, S.(f ++ reverse r), 0, lazy S.Nil)
    else (lenf, f, lenr, r)

  let snoc x (lenf, f, lenr, r) = check (lenf, f, lenr + 1, lazy (S.Cons (x, r)))

  let head (_, f, _, _) =
    match Lazy.force f with
    | S.Nil ->
        raise (Failure "Queue is empty")
    | S.Cons (x, f') ->
        x

  let tail (lenf, f, lenr, r) =
    match Lazy.force f with
    | S.Nil ->
        raise (Failure "Queue is empty")
    | S.Cons (x, f') ->
        check (lenf - 1, f', lenr, r)
end
