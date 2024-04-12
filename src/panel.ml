open Brr

let showPanel = Jv.get Jv.global "__CM__showPanel"

type t = Jv.t

include (Jv.Id : Jv.CONV with type t := t)

let create ?mount ?update ?top ?pos dom =
  let o = Jv.obj [||] in
  Jv.set_if_some o "mount" (Option.map Jv.repr mount);
  Jv.set_if_some o "update"
    (Option.map
       (fun u ->
         let u' jv = u (View.Update.of_jv jv) in
         u')
       update
    |> Option.map (Jv.callback ~arity:1));
  Jv.Bool.set_if_some o "top" top;
  Jv.Int.set_if_some o "pos" pos;
  Jv.set o "dom" (El.to_jv dom);
  o

module Constructor : sig
  type pc = (View.t -> t) option

  val of_jv : Jv.t -> pc
  val to_jv : pc -> Jv.t
end = struct
  type pc = (View.t -> t) option

  let to_jv (f : pc) =
    match f with
    | Some x ->
        Brr.Console.log [ Jstr.v "Got a callback!" ];
        let callback v =
          let v = View.of_jv v in
          to_jv (x v)
        in
        Jv.callback ~arity:1 callback
    | None ->
        Brr.Console.log [ Jstr.v "No callback!" ];
        Jv.null

  let of_jv _jv = assert false
end

let showPanel : (Constructor.pc, Jv.t) Facet.t =
  let iconv = Constructor.{ Types.of_jv; to_jv } in
  Types.Facet.create iconv showPanel
