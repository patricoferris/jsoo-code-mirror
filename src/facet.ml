include Types.Facet

let of_ : ('i, 'o) t -> 'i -> Extension.t =
 fun v i -> Jv.call (to_jv v) "of" [| (to_conv v).to_jv i |] |> Extension.of_jv

let from : ('i, 'o) t -> 'a Field.field -> Extension.t =
 fun v f -> Jv.call (to_jv v) "from" [| Field.to_jv f |] |> Extension.of_jv

let from' : ('i, 'o) t -> 'a Field.field -> ('a -> 'i) -> Extension.t =
 fun v f fn ->
  let wrapped_fn x = fn x |> (to_conv v).to_jv in
  Jv.call (to_jv v) "from" [| Field.to_jv f; Jv.callback ~arity:1 wrapped_fn |]
  |> Extension.of_jv
