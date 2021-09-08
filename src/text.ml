module Line = struct
  type t = Jv.t

  let from t = Jv.Int.get t "from"

  let to_ t = Jv.Int.get t "to"

  let number t = Jv.Int.get t "number"

  let text t = Jv.Jstr.get t "text"

  let length t = Jv.Int.get t "length"
end
