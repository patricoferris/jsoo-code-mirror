include Types.Effect

let state_effect = lazy (Jv.get Jv.global "__CM__StateEffect")

let define : type a. (a -> Jv.t) -> (Jv.t -> a) -> a ty =
 fun to_jv of_jv ->
  let jv = Jv.call (Lazy.force state_effect) "define" [||] in
  ty_of_jv { Types.to_jv; of_jv } jv

let is : t -> 'a ty -> bool =
 fun v ty -> Jv.call (to_jv v) "is" [| ty_to_jv ty |] |> Jv.to_bool

let value t ty =
  if is t ty then
    let c = conv_of_ty ty in
    Some (Jv.get (to_jv t) "value" |> c.of_jv)
  else None

let of_ ty v =
  let conv = conv_of_ty ty in
  Jv.call (ty_to_jv ty) "of" [| conv.to_jv v |] |> of_jv
