type 'a conv = { to_jv : 'a -> Jv.t; of_jv : Jv.t -> 'a }

module Effect : sig
  type t
  type 'a ty

  include Jv.CONV with type t := t

  val ty_to_jv : 'a ty -> Jv.t
  val conv_of_ty : 'a ty -> 'a conv
  val ty_of_jv : 'a conv -> Jv.t -> 'a ty
end

module rec State : sig
  type t

  include Jv.CONV with type t := t
end

and Transaction : sig
  type t

  include Jv.CONV with type t := t
end

and Field : sig
  type 'a field

  val extension : 'a field -> Extension.t
  val conv : 'a field -> 'a conv
  val to_jv : 'a field -> Jv.t
  val of_jv : 'a conv -> Jv.t -> 'a field
end

and Config : sig
  type t

  include Jv.CONV with type t := t
  (* TODO: Add selection *)

  val undefined : t
end

and Facet : sig
  type ('input, 'output) t

  val to_jv : ('input, 'output) t -> Jv.t
  val to_conv : ('input, 'output) t -> 'input conv
  val create : 'input conv -> Jv.t -> ('input, 'output) t
end
