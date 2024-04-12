type 'a conv = 'a Editor_impl.conv = { to_jv : 'a -> Jv.t; of_jv : Jv.t -> 'a }

module Effect = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type 'a ty = 'a conv * Jv.t

  let ty_to_jv : 'a ty -> Jv.t = fun (_, t) -> t
  let conv_of_ty (conv, _) = conv
  let ty_of_jv conv jv = (conv, jv)
end

module Transaction = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

module State = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

module Field = struct
  type 'a field = 'a conv * Jv.t

  let of_jv conv jv = (conv, jv)
  let extension (_, v) = Extension.of_jv v
  let to_jv (_, v) = v
  let conv (c, _) = c
end

module Config = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let undefined : t = Jv.undefined
end

module Facet = struct
  type ('input, 'output) t = 'input conv * Jv.t

  let to_jv (_, v) = v
  let create iconv v = (iconv, v)
  let to_conv (c, _) = c
end
