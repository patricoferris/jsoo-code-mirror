include Types.Config

let create ?doc ?selection ?extensions () =
  let o = Jv.obj [||] in
  Jv.Jstr.set_if_some o "doc" doc;
  Jv.set_if_some o "selection" selection;
  Jv.set_if_some o "extensions"
    (Option.map (Jv.of_array Extension.to_jv) extensions);
  of_jv o
