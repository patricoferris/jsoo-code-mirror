let g = Jv.get Jv.global "__CM__stream_parser"

module Language = struct
  type t = Jv.t

  let g = Jv.get g "StreamLanguage"

  let define (l : Language.t) = 
    Jv.call g "define" [| Language.to_jv l |] |> Extension.of_jv
end
