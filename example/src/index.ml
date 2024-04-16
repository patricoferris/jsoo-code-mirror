open Code_mirror
open Brr

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let init ?doc ?(exts = [||]) () =
  let config =
    Config.create ?doc ~extensions:(Array.concat [ [| basic_setup |]; exts ]) ()
  in
  let state = State.create ~config () in
  let opts = View.opts ~state ~parent:(Document.body G.document) () in
  let view : View.t = View.create ~opts () in
  (state, view)

let update dom v =
  let st = View.Update.state v in
  let doc = State.doc st in
  let length = Text.length doc in
  El.set_children dom [ El.txt' (string_of_int length) ]

let panel_constructor (_v : View.t) =
  let dom = Brr.El.div [ Brr.El.txt (Jstr.v "Hello, panel world") ] in
  Panel.create ~update:(update dom) dom

let _ =
  let toggleHelp = Effect.define Jv.of_bool Jv.to_bool in

  let state_update cur t =
    let effects = Transaction.effects t in
    List.fold_right
      (fun e cur ->
        match Effect.value e toggleHelp with Some b -> b | _ -> cur)
      effects cur
  in

  let provide field =
    Facet.from' Panel.showPanel field (fun b ->
        if b then Some panel_constructor else None)
  in

  let help_state =
    Field.define Jv.of_bool Jv.to_bool
      ~create:(fun _ -> false)
      ~provide ~update:state_update
  in

  let run v =
    let cur = State.field (View.state v) help_state in
    let eff = Effect.of_ toggleHelp (not cur) in
    let transaction = Transaction.create ~effects:[ eff ] () in
    View.dispatch v transaction;
    true
  in

  let keymap = Keymap.create ~key:"F1" ~run () in

  let ext = Facet.of_ Keymap.keymap keymap in

  let _editor = init ~exts:[| ext; Editor.Field.extension help_state |] () in
  ()
