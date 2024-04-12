type 'a conv = {
  to_jv : 'a -> Jv.t;
  of_jv : Jv.t -> 'a;
}

module Effect : sig
  type t
  
  include Jv.CONV with type t := t

  type 'a ty 

  val define : ('a -> Jv.t) -> (Jv.t -> 'a) -> 'a ty

  val is : t -> 'a ty -> bool

  val value : t -> 'a ty -> 'a option

  val of_ : 'a ty -> 'a -> t
end = struct

  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
  
  type 'a ty = 'a conv * Jv.t

  let define : type a. (a -> Jv.t) -> (Jv.t -> a) -> a ty =
    fun to_jv of_jv ->
      let state_effect = Jv.get Jv.global "__CM__StateEffect" in
      let jv = Jv.call state_effect "define" [||] in
      ({to_jv; of_jv}, jv)

  let is : t -> 'a ty -> bool =
    fun v (_, t2) -> Jv.call v "is" [| t2 |] |> Jv.to_bool

  let value t ty =
    if is t ty
    then
      let (c, _) = ty in
      Some (Jv.get t "value" |> c.of_jv)
    else
      None

  let of_ (c, t) v = Jv.call t "of" [| c.to_jv v |]
end

module State = struct

  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)


  module Transaction = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let effects : t -> Effect.t list = fun v -> 
      Jv.get v "effects" |> Jv.to_list Effect.of_jv

    let create ?(effects = []) () =
      let o = Jv.obj [||] in
      Jv.set o "effects" (Jv.of_list Effect.to_jv effects);
      o
  
  end

  module Field 
  : sig
    type 'a field

    val extension : 'a field -> Extension.t

    val define : 'a. ?compare:('a -> 'a -> bool) -> ?provide:('a field -> Extension.t) -> ('a -> Jv.t) -> (Jv.t -> 'a) -> create:(t -> 'a) -> update:('a -> Transaction.t -> 'a) -> 'a field
    
    val to_jv : 'a field -> Jv.t 

    val conv : 'a field -> 'a conv
  end 
  = struct
    type 'a field = 'a conv * Jv.t

    let define : type a. ?compare:(a -> a -> bool) -> ?provide:(a field -> Extension.t) -> (a -> Jv.t) -> (Jv.t -> a) -> create:(t -> a) -> update:(a -> Transaction.t -> a) -> a field = fun ?compare ?provide v_to_jv v_of_jv ~create ~update ->
      let state_field = Jv.get Jv.global "__CM__StateField" in
      let update_wrapper v t =
        Brr.Console.log [Jstr.v "update wrapper"];
        let v' = v_of_jv v in
        let t = Transaction.of_jv t in
        let new_v = update v' t in
        Brr.Console.log [Jstr.v "finished update wrapper"];
        v_to_jv new_v
      in
      let create_wrapper st =
        Brr.Console.log [Jstr.v "create wrapper"];
        let st = of_jv st in
        let v' = create st in
        v_to_jv v'
      in
      let provide = Option.map (fun f v ->
        Brr.Console.log [Jstr.v "provide wrapper"];
        let v' = ({to_jv = v_to_jv; of_jv = v_of_jv}, v) in 
        let e = f v' in
        Extension.to_jv e
      ) provide in
      let o = Jv.obj [||] in
      Jv.set_if_some o "compare" (Option.map (Jv.callback ~arity:2) compare);
      Jv.set_if_some o "provide" (Option.map (Jv.callback ~arity:1) provide);
      Jv.set o "update" (Jv.callback ~arity:2 update_wrapper);
      Jv.set o "create" (Jv.callback ~arity:1 create_wrapper);
      let jv = Jv.call state_field "define" [|o|] in
      ({to_jv = v_to_jv; of_jv=v_of_jv}, jv)

      let extension (_, v) = Extension.of_jv v


      let to_jv (_, v) = v

      let conv (c, _) = c
  end


  module Config = struct
    type t = Jv.t

    let create ?doc ?selection ?extensions () =
      let o = Jv.obj [||] in
      Jv.Jstr.set_if_some o "doc" doc;
      Jv.set_if_some o "selection" selection;
      Jv.set_if_some o "extensions"
        (Option.map (Jv.of_array Extension.to_jv) extensions);
      o
  end

  module Facet 
   : sig
    type ('input, 'output) t

    val to_jv : ('input, 'output) t -> Jv.t

    val of_ : ('input, 'output) t -> 'input -> Extension.t

    val from : ('input, 'output) t -> 'a Field.field -> Extension.t

    val from' : ('input, 'output) t -> 'a Field.field -> ('a -> 'input) -> Extension.t

    val create : 'input conv -> Jv.t -> ('input, 'output) t
  end  
  = struct

    type ('input, 'output) t = 'input conv * Jv.t

    let to_jv (_, v) = v

    let create iconv v = (iconv, v)
  
    let of_ : ('i, 'o) t -> 'i -> Extension.t =
      fun (iconv, jv) i -> Jv.call jv "of" [| iconv.to_jv i |] |> Extension.of_jv

    let from : ('i, 'o) t -> 'a Field.field -> Extension.t = fun (_, t) f ->
      Jv.call t "from" [| Field.to_jv f |] |> Extension.of_jv

    let from' : ('i, 'o) t -> 'a Field.field -> ('a -> 'i) -> Extension.t = fun (iconv, t) f fn ->
      let wrapped_fn v = fn v |> iconv.to_jv in
      Jv.call t "from" [| Field.to_jv f; Jv.callback ~arity:1 wrapped_fn |] |> Extension.of_jv
  end
 

  let create ?(config = Jv.undefined) () =
    let editor_state = Jv.get Jv.global "__CM__state" in
    Jv.call editor_state "create" [| config |]


  let doc t = Jv.get t "doc" |> Text.of_jv

  let field t f =
    let c = Field.conv f in
    Jv.call t "field" [| Field.to_jv f |] |> c.of_jv
end

module View = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?state ?parent ?root ?dispatch () =
    let o = Jv.obj [||] in
    Jv.set_if_some o "state" state;
    Jv.set_if_some o "root" (Option.map Brr.Document.to_jv root);
    Jv.set_if_some o "dispatch" dispatch;
    Jv.set_if_some o "parent" (Option.map Brr.El.to_jv parent);
    o

  let g = Jv.get Jv.global "__CM__view"
  let create ?(opts = Jv.undefined) () = Jv.new' g [| opts |]
  let state t = Jv.get t "state" |> State.of_jv
  let set_state t v = Jv.call t "setState" [| State.to_jv v |] |> ignore

  module Update = struct
    type t = Jv.t

    let state t = State.of_jv @@ Jv.get t "state"

    include (Jv.Id : Jv.CONV with type t := t)
  end

  let dom t = Jv.get t "dom" |> Brr.El.of_jv

  let update_listener () : (Update.t -> unit, Jv.t) State.Facet.t =
    let jv_of_fn f =
      Jv.callback ~arity:1 (fun u -> f (Update.of_jv u))
    in
    let iconv = { to_jv = jv_of_fn; of_jv = fun _ -> assert false } in
    let jv = Jv.get g "updateListener" in
    State.Facet.create iconv jv

  let dispatch : t -> State.Transaction.t -> unit = fun t tr -> Jv.call t "dispatch" [| State.Transaction.to_jv tr |] |> ignore

  let line_wrapping () = Jv.get g "lineWrapping" |> Extension.of_jv
end

module Keymap : sig
  type t

  include Jv.CONV with type t := t

  type command = View.t -> bool

  val create : ?key:string -> ?run:command -> unit -> t

end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type command = View.t -> bool

  let create ?key ?run () =
    let o = Jv.obj [||] in
    let key = Option.map Jstr.v key in
    let run = Option.map (fun f ->
      Jv.callback ~arity:1 (fun v -> f (View.of_jv v) |> Jv.of_bool)) run in
    Jv.Jstr.set_if_some o "key" key;
    Jv.set_if_some o "run" run;
    o
end

let keymap : (Keymap.t, Jv.t) State.Facet.t =
  let iconv = Keymap.{ of_jv; to_jv } in
  let jv = Jv.get Jv.global "__CM__keymap" in
  State.Facet.create iconv jv




