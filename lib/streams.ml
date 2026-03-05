module Make (Ord : Set.OrderedType) = struct
  type stream = stream_cell Lazy.t

  and stream_cell = Nil | Cons of Ord.t * stream

  let rec ( ++ ) stream t =
    lazy
      ( match Lazy.force stream with
      | Nil ->
          Lazy.force t
      | Cons (x, s) ->
          Cons (x, s ++ t) )

  let rec take n s =
    lazy
      ( match n with
      | 0 ->
          Nil
      | n -> (
        match Lazy.force s with
        | Nil ->
            Nil
        | Cons (x, s') ->
            Cons (x, take (n - 1) s') ) )

  let rec drop n s =
    lazy
      ( match n with
      | 0 ->
          Lazy.force s
      | n -> (
        match Lazy.force s with
        | Nil ->
            Nil
        | Cons (x, s') ->
            Lazy.force (drop (n - 1) s') ) )

  let reverse s =
    let rec reverse' s' r =
      lazy
        ( match Lazy.force s' with
        | Nil ->
            r
        | Cons (x, s'') ->
            Lazy.force (reverse' s'' (Cons (x, lazy r))) )
    in
    reverse' s Nil
end
