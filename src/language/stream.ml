let g = Jv.get Jv.global "__CM__language"

module Language = struct
  type t

  include (Jv.Id : Jv.CONV with type t := t)

  let g = Jv.get g "StreamLanguage"

  let define (l : t) =
    Jv.call g "define" [| to_jv l |] |> Cm_state.Extension.of_jv
end
