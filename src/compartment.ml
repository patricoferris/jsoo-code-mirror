type t = Jv.t

include (Jv.Id : Jv.CONV with type t := t)

let compartment = Jv.get Jv.global "__CM__compartment"
let make () = Jv.new' compartment [||]

let of' t extensions =
  Jv.call t "of" [| Jv.of_list Extension.to_jv extensions |] |> Extension.of_jv

let reconfigure t ext =
  let state_effect =
    Jv.call t "reconfigure" [| Jv.of_list Extension.to_jv ext |]
  in
  Jv.obj [| ("effects", state_effect) |] |> Editor.View.Transaction.of_jv
