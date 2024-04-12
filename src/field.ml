(* Field *)
include Types.Field

let state_field = lazy (Jv.get Jv.global "__CM__StateField")

let define :
    type a.
    ?compare:(a -> a -> bool) ->
    ?provide:(a field -> Extension.t) ->
    (a -> Jv.t) ->
    (Jv.t -> a) ->
    create:(State.t -> a) ->
    update:(a -> Transaction.t -> a) ->
    a field =
 fun ?compare ?provide v_to_jv v_of_jv ~create ~update ->
  let update_wrapper v t =
    update (v_of_jv v) (Transaction.of_jv t) |> v_to_jv
  in
  let create_wrapper st = create (State.of_jv st) |> v_to_jv in
  let provide =
    Option.map
      (fun f v ->
        f (Types.Field.of_jv { Types.to_jv = v_to_jv; of_jv = v_of_jv } v)
        |> Extension.to_jv)
      provide
  in
  let o = Jv.obj [||] in
  Jv.set_if_some o "compare" (Option.map (Jv.callback ~arity:2) compare);
  Jv.set_if_some o "provide" (Option.map (Jv.callback ~arity:1) provide);
  Jv.set o "update" (Jv.callback ~arity:2 update_wrapper);
  Jv.set o "create" (Jv.callback ~arity:1 create_wrapper);
  let jv = Jv.call (Lazy.force state_field) "define" [| o |] in
  Types.Field.of_jv { Types.to_jv = v_to_jv; of_jv = v_of_jv } jv
