include Types.State

let editor_state = lazy (Jv.get Jv.global "__CM__state")

let create : ?config:Types.Config.t -> unit -> t =
 fun ?(config = Types.Config.undefined) () ->
  Jv.call (Lazy.force editor_state) "create" [| Types.Config.to_jv config |]
  |> of_jv

let doc (t : t) = Jv.get (to_jv t) "doc" |> Text.of_jv

let field (t : t) (f : 'a Types.Field.field) =
  let c = Types.Field.conv f in
  Jv.call (to_jv t) "field" [| Types.Field.to_jv f |] |> c.of_jv
