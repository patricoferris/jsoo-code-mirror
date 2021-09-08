module State = struct 

  module Config = struct 
    type t = Jv.t

    let create ?doc ?selection ?extensions () = 
      let o = Jv.obj [||] in
      Jv.Jstr.set_if_some o "doc" doc;
      Jv.set_if_some o "selection" selection;
      Jv.set_if_some o "extensions" (Option.map (Jv.of_array Extension.to_jv) extensions);
      o
  end

  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?(config = Jv.undefined) () = 
    let editor_state = Jv.get Jv.global "__CM__state" in 
    Jv.call editor_state "create" [| config |]
end 

module View = struct 
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t 

  let opts ?state ?root ?dispatch ?parent () = 
    let o = Jv.obj [||] in
    Jv.set_if_some o "state" state;
    Jv.set_if_some o "root" (Option.map Brr.Document.to_jv root);
    Jv.set_if_some o "dispatch" dispatch;
    Jv.set_if_some o "parent" (Option.map Brr.El.to_jv parent);
    o

  let create ?(opts = Jv.undefined) () = 
    Jv.new' (Jv.get Jv.global "__CM__view") [| opts |]

    (* TODO *)
  module Update = struct 
    type t = Jv.t 
    include (Jv.Id : Jv.CONV with type t := t)
  end
end