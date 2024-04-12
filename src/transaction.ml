(* Transactions *)

include Types.Transaction

let effects : t -> Types.Effect.t list =
 fun v -> Jv.get (to_jv v) "effects" |> Jv.to_list Types.Effect.of_jv

let create ?(effects = []) () =
  let o = Jv.obj [||] in
  Jv.set o "effects" (Jv.of_list Types.Effect.to_jv effects);
  of_jv o
