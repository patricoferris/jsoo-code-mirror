let keymap = lazy (Jv.get Jv.global "__CM__keymap")

type t = Jv.t

include (Jv.Id : Jv.CONV with type t := t)

type command = View.t -> bool

let create ?key ?run () =
  let o = Jv.obj [||] in
  let key = Option.map Jstr.v key in
  let run =
    Option.map
      (fun f -> Jv.callback ~arity:1 (fun v -> f (View.of_jv v) |> Jv.of_bool))
      run
  in
  Jv.Jstr.set_if_some o "key" key;
  Jv.set_if_some o "run" run;
  o

let keymap : (t, Jv.t) Facet.t =
  let iconv = Types.{ of_jv; to_jv } in
  Facet.create iconv (Lazy.force keymap)
