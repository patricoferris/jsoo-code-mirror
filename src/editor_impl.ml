(* editor_impl *)

type 'a conv = { to_jv : 'a -> Jv.t; of_jv : Jv.t -> 'a }

module type Transaction = sig
  type t

  include Jv.CONV with type t := t
end

module type State = sig
  type t

  include Jv.CONV with type t := t
end

module type Field = sig
  type 'a field

  val extension : 'a field -> Extension.t
  val of_jv : 'a conv -> Jv.t -> 'a field
  val conv : 'a field -> 'a conv
  val to_jv : 'a field -> Jv.t
end

module type Config = sig
  type t

  include Jv.CONV with type t := t

  val undefined : t
end

module type Facet = sig
  type ('input, 'output) t

  val to_jv : ('input, 'output) t -> Jv.t
  val to_conv : ('input, 'output) t -> 'input conv
  val create : 'input conv -> Jv.t -> ('input, 'output) t
end
