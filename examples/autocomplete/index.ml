(* Autocompletion from a fixed list. print_endline is applied as text;
   List.map is applied by a function, which also inserts an argument
   placeholder. Accept a completion with Enter or by clicking it. *)

open Code_mirror
open State
open View
open Brr
open Autocomplete

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let print_endline_ =
  Completion.create ~label:"print_endline" ~type_:"function"
    ~apply:(Completion.Text "print_endline \"\"") ()

let list_map =
  Completion.create ~label:"List.map" ~type_:"function"
    ~apply:
      (Completion.Fn
         (fun view _completion ~from ~to_ ->
           EditorView.dispatch view
             (TransactionSpec.create
                ~changes:
                  (ChangeSpec.create ~from ~to_ ~insert:"List.map f l" ())
                ())))
    ()

let () =
  let completions =
    config ~override:[ Source.from_list [ print_endline_; list_map ] ] ()
  in
  let config =
    EditorStateConfig.create ~doc:"(* Type pr or Li *)\n"
      ~extensions:
        (Extension.of_list [ basic_setup; create ~config:completions () ])
      ()
  in
  let state = EditorState.create ~config () in
  ignore
    (EditorView.create
       ~config:
         (EditorViewConfig.create ~state ~parent:(Document.body G.document) ())
       ())
