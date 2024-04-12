type t = Jv.t

let view = lazy (Jv.get Jv.global "__CM__view")

include (Jv.Id : Jv.CONV with type t := t)

type opts = Jv.t

let opts ?(state : State.t option) ?parent ?root ?dispatch () =
  let o = Jv.obj [||] in
  Jv.set_if_some o "state" (Option.map State.to_jv state);
  Jv.set_if_some o "root" (Option.map Brr.Document.to_jv root);
  Jv.set_if_some o "dispatch" dispatch;
  Jv.set_if_some o "parent" (Option.map Brr.El.to_jv parent);
  o

let create ?(opts = Jv.undefined) () = Jv.new' (Lazy.force view) [| opts |]
let state t = Jv.get t "state" |> State.of_jv
let set_state t v = Jv.call t "setState" [| State.to_jv v |] |> ignore

module Update = struct
  type t = Jv.t

  let state t = State.of_jv @@ Jv.get t "state"

  include (Jv.Id : Jv.CONV with type t := t)
end

let dom t = Jv.get t "dom" |> Brr.El.of_jv

let update_listener () : (Update.t -> unit, Jv.t) Facet.t =
  let jv_of_fn f = Jv.callback ~arity:1 (fun u -> f (Update.of_jv u)) in
  let iconv = { Types.to_jv = jv_of_fn; of_jv = (fun _ -> assert false) } in
  let jv = Jv.get (Lazy.force view) "updateListener" in
  Facet.create iconv jv

let dispatch : t -> Transaction.t -> unit =
 fun t tr -> Jv.call t "dispatch" [| Transaction.to_jv tr |] |> ignore

let line_wrapping () =
  Jv.get (Lazy.force view) "lineWrapping" |> Extension.of_jv
