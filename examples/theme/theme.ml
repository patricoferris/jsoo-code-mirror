open Code_mirror
open Brr

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv
let dracula = Jv.get Jv.global "__CM__theme_dracula" |> Extension.of_jv

let init ?doc () =
  let config =
    State.EditorStateConfig.create ?doc ~extensions:[dracula; basic_setup] ()
  in
  let state = State.EditorState.create ~config () in
  let config =
    View.EditorViewConfig.create ~state ~parent:(Document.body G.document) ()
  in
  let view : View.EditorView.t = View.EditorView.create ~config () in
  (state, view)

let _ =
  let _ =
    init ~doc:"Hello, world2! Some test\nSome more text\n" ()
  in
  ()
