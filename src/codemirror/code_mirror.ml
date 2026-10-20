(* The codemirror package: basicSetup. *)

let basic_setup : Cm_state.Extension.t =
  Cm_state.Extension.of_jv (Jv.get Jv.global "__CM__basic_setup")
