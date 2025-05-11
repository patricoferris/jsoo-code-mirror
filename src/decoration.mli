module Widget : sig
  type t

  include Jv.CONV with type t := t

  val make : (unit -> Brr.El.t) -> t
end

type t

include Jv.CONV with type t := t

val widget : ?block:bool -> ?side:int -> Widget.t -> t

module Range : sig
  type t

  include Jv.CONV with type t := t
end

val range : from:int -> to_:int -> t -> Range.t

module Range_set : sig
  type t

  include Jv.CONV with type t := t

  val of' : Range.t array -> t
end
