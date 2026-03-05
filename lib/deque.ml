module type S = sig
  type t

  type elt

  val empty : t

  val isEmpty : t -> bool

  val cons : elt -> t -> t

  val snoc : elt -> t -> t

  val head : t -> elt

  val tail : t -> t

  val last : t -> elt

  val init : t -> t
end

module type Make = functor (Ord : Set.OrderedType) -> S with type elt = Ord.t

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  type t = elt list * elt list

  let empty = ([], [])

  let isEmpty = function [], [] -> true | _ -> false

  let split_at n lst =
    let rec aux n acc = function
      | x :: xs ->
          if List.length xs == n then (List.rev (x :: acc), xs)
          else aux n (x :: acc) xs
      | [] ->
          (List.rev acc, [])
    in
    aux n [] lst

  let checkf = function
    | [], r ->
        let half = (List.length r + 1) / 2 in
        split_at half r
    | t, [] ->
        let half = (List.length t + 1) / 2 in
        split_at half t
    | t, r ->
        (t, r)

  let snoc x (t, r) = checkf (t, x :: r)

  let head = function
    | [], _ ->
        raise (Failure "Deque is empty")
    | x :: t, r ->
        x

  let tail = function
    | [], _ ->
        raise (Failure "Deque is empty")
    | x :: t, r ->
        checkf (t, r)

  let cons x (t, r) = checkf (x :: t, r)

  let init = function
    | [], _ ->
        raise (Failure "Deque is empty")
    | t, [] ->
        ([], [])
    | t, x :: r ->
        checkf (t, r)

  let last = function
    | [], _ ->
        raise (Failure "Deque is empty")
    | x :: t, [] ->
        x
    | t, x :: r ->
        x
end
