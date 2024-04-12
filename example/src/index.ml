open Code_mirror
open Brr

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let init ?doc ?(exts = [||]) () =
  let open Editor in
  let config =
    State.Config.create ?doc
      ~extensions:(Array.concat [ [| basic_setup |]; exts ])
      ()
  in
  let state = State.create ~config () in
  let opts = View.opts ~state ~parent:(Document.body G.document) () in
  let view : View.t = View.create ~opts () in
  (state, view)


module PanelConstructor : sig
  type t = (Editor.View.t -> Panel.t) option

  val of_jv : Jv.t -> t
  val to_jv : t -> Jv.t
end = struct
  type t = (Editor.View.t -> Panel.t) option

  let to_jv (f : t) =
    match f with
    | Some x ->
      Brr.Console.log [Jstr.v "Got a callback!"];
      let callback v =
        let v = Editor.View.of_jv v in
        Panel.to_jv (x v)
      in
      Jv.callback ~arity:1 callback
    | None ->
      Brr.Console.log [Jstr.v "No callback!"];
      Jv.null
  
  let of_jv _jv = assert false
end


let showPanel : (PanelConstructor.t, Jv.t) Editor.State.Facet.t =
  let iconv = PanelConstructor.{ Editor.of_jv; to_jv } in
  let jv = Jv.get Jv.global "__CM__showPanel" in
  Editor.State.Facet.create iconv jv


let update dom v =
  let st = Editor.View.Update.state v in
  let doc = Editor.State.doc st in
  let length = Text.length doc in
  El.set_children dom [El.txt' (string_of_int length)]

let panel_constructor (_v : Editor.View.t) =
  let dom = Brr.El.div [Brr.El.txt (Jstr.v "Hello, panel world")] in
  Panel.create ~update:(update dom) dom

let _ =
  let toggleHelp = Editor.Effect.define Jv.of_bool Jv.to_bool in

  let state_update cur t =
    let effects = Editor.State.Transaction.effects t in
    List.fold_right (fun e cur ->
      match Editor.Effect.value e toggleHelp with
      | Some b -> b
      | _ -> cur
        ) effects cur
  in

  let provide field =
      Editor.State.Facet.from' showPanel field (fun b ->
        Brr.Console.log [Jstr.v ("Here we are: b=" ^ string_of_bool b)];
        if b then Some panel_constructor else None
        )
  in

  let help_state = Editor.State.Field.define Jv.of_bool Jv.to_bool ~create:(fun _ -> false) ~provide ~update:state_update in
  
  let run v =
    let cur = Editor.(State.field (View.state v) help_state) in
    let eff = Editor.Effect.of_ toggleHelp (not cur) in 
    let transaction = Editor.State.Transaction.create ~effects:[eff] () in
    Editor.View.dispatch v transaction;
    Console.log [Jstr.v "F1 pressed"];
    true
  in

  let keymap = Editor.Keymap.create ~key:"F1" ~run () in

  let ext = Editor.State.Facet.of_ Editor.keymap keymap in

  let _editor = init ~exts:[| ext;
   Editor.State.Field.extension help_state
   
   |] () in
  ()

