module Make (Ord : Set.OrderedType) : Queue.S with type elt = Ord.t = struct
  type elt = Ord.t

  type t = elt list * elt list

  let empty = ([], [])

  let isEmpty = function [], _ -> true | qs -> false

  let checkf = function [], r -> (List.rev r, []) | q -> q

  let snoc x (t, r) = checkf (t, x :: r)

  let head = function [], _ -> raise (Failure "Empty queue.") | x :: t, r -> x

  let tail = function
    | [], _ ->
        raise (Failure "Empty queue.")
    | x :: t, r ->
        checkf (t, r)
end
