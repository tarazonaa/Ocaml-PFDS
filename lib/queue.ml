module type S = sig
  type t

  type elt

  val empty : t

  val isEmpty : t -> bool

  val snoc : elt -> t -> t

  val head : t -> elt

  val tail : t -> t
end

module type Make = functor (Ord : Set.OrderedType) -> S with type elt = Ord.t
