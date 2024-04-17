include Types.State

module EditorStateConfig = struct
  include EditorStateConfig

  let create : ?doc:string -> ?selection:EditorSelection.t -> ?extensions:Extension.t list -> unit -> t = fun ?doc ?selection ?extensions () ->
    let o = Jv.obj [||] in
    Jv.Jstr.set_if_some o "doc" (Option.map Jstr.of_string doc);
    Jv.set_if_some o "selection" (Option.map EditorSelection.to_jv selection);
    Jv.set_if_some o "extensions"
      (Option.map (Jv.of_list Extension.to_jv) extensions);
    of_jv o
end

module EditorState = struct
  include EditorState

  let editor_state = lazy (Jv.get Jv.global "__CM__state")

  let create : ?config:EditorStateConfig.t -> unit -> t =
   fun ?(config = EditorStateConfig.undefined) () ->
    Jv.call (Lazy.force editor_state) "create"
      [| EditorStateConfig.to_jv config |]
    |> of_jv

  let doc (t : t) = Jv.get (to_jv t) "doc" |> Text.of_jv

  let field (t : t) (f : 'a StateField.t) =
    let c = StateField.conv f in
    Jv.call (to_jv t) "field" [| StateField.to_jv f |] |> c.of_jv
end

module SelectionRange : sig
  type t

  include Jv.CONV with type t := t

  val from : t -> int

  val to_ : t -> int

  val anchor : t -> int

  val head : t -> int

  val empty : t -> bool

  val assoc : t -> int

end = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
  let from : t -> int = fun t -> Jv.Int.get t "from"
  let to_ : t -> int = fun t -> Jv.Int.get t "to"
  let anchor : t -> int = fun t -> Jv.Int.get t "anchor"
  let head : t -> int = fun t -> Jv.Int.get t "head"
  let empty : t -> bool = fun t -> Jv.Bool.get t "empty"
  let assoc : t -> int = fun t -> Jv.Int.get t "assoc"
end

module EditorSelection = struct
  include EditorSelection

  let editor_selection = lazy (Jv.get Jv.global "__CM__EditorSelection")

  let create : ranges:SelectionRange.t list -> ?main_index:int -> unit -> t =
   fun ~ranges ?(main_index = 0) () ->
    Jv.call (Lazy.force editor_selection) "create" (Array.of_list ((List.map SelectionRange.to_jv ranges) @ [ Jv.of_int main_index ])) |> of_jv

  let cursor : pos:int -> ?assoc:int -> ?bidiLevel:int -> ?goalColumn:int -> unit -> SelectionRange.t = fun
    ~pos ?(assoc=0) ?bidiLevel ?goalColumn () ->
      let opt x = match x with | Some x -> Jv.of_int x | None -> Jv.undefined in
      Jv.call (Lazy.force editor_selection) "cursor"
        [| Jv.of_int pos; Jv.of_int assoc; opt bidiLevel; opt goalColumn |] |> SelectionRange.of_jv

  let range : anchor:int -> head:int -> ?goalColumn:int -> ?bidiLevel:int -> unit -> SelectionRange.t = fun
    ~anchor ~head ?goalColumn ?bidiLevel () ->
      let opt x = match x with | Some x -> Jv.of_int x | None -> Jv.undefined in
      Jv.call (Lazy.force editor_selection) "range"
        [| Jv.of_int anchor; Jv.of_int head; opt goalColumn; opt bidiLevel |] |> SelectionRange.of_jv
end

module Text = struct
  include Text

  let length (t : t) = Jv.Int.get (to_jv t) "length"
  let line n (t : t) = Jv.call (to_jv t) "line" [| Jv.of_int n |]

  let to_jstr_array (t : t) =
    Jv.call (to_jv t) "toJSON" [||] |> Jv.to_jstr_array
end

module Line = struct
  type t = Jv.t

  let from t = Jv.Int.get t "from"
  let to_ t = Jv.Int.get t "to"
  let number t = Jv.Int.get t "number"
  let text t = Jv.Jstr.get t "text"
  let length t = Jv.Int.get t "length"
end

module StateEffect = struct
  include StateEffect

  let state_effect = lazy (Jv.get Jv.global "__CM__StateEffect")

  let define : type a. (a -> Jv.t) -> (Jv.t -> a) -> a ty =
   fun to_jv of_jv ->
    let jv = Jv.call (Lazy.force state_effect) "define" [||] in
    ty_of_jv { Types.to_jv; of_jv } jv

  let is : t -> 'a ty -> bool =
   fun v ty -> Jv.call (to_jv v) "is" [| ty_to_jv ty |] |> Jv.to_bool

  let value t ty =
    if is t ty then
      let c = conv_of_ty ty in
      Some (Jv.get (to_jv t) "value" |> c.of_jv)
    else None

  let of_ ty v =
    let conv = conv_of_ty ty in
    Jv.call (ty_to_jv ty) "of" [| conv.to_jv v |] |> of_jv
end

module StateField = struct
  include StateField

  let state_field = lazy (Jv.get Jv.global "__CM__StateField")

  let define :
      type a.
      ?compare:(a -> a -> bool) ->
      ?provide:(a t -> Extension.t) ->
      (a -> Jv.t) ->
      (Jv.t -> a) ->
      create:(EditorState.t -> a) ->
      update:(a -> Transaction.t -> a) ->
      a t =
   fun ?compare ?provide v_to_jv v_of_jv ~create ~update ->
    let update_wrapper v t =
      update (v_of_jv v) (Transaction.of_jv t) |> v_to_jv
    in
    let create_wrapper st = create (EditorState.of_jv st) |> v_to_jv in
    let provide =
      Option.map
        (fun f v ->
          f (StateField.of_jv { Types.to_jv = v_to_jv; of_jv = v_of_jv } v)
          |> Extension.to_jv)
        provide
    in
    let o = Jv.obj [||] in
    Jv.set_if_some o "compare" (Option.map (Jv.callback ~arity:2) compare);
    Jv.set_if_some o "provide" (Option.map (Jv.callback ~arity:1) provide);
    Jv.set o "update" (Jv.callback ~arity:2 update_wrapper);
    Jv.set o "create" (Jv.callback ~arity:1 create_wrapper);
    let jv = Jv.call (Lazy.force state_field) "define" [| o |] in
    StateField.of_jv { Types.to_jv = v_to_jv; of_jv = v_of_jv } jv
end

module Facet = struct
  include Facet

  let of_ : ('i, 'o) t -> 'i -> Extension.t =
   fun v i ->
    Jv.call (to_jv v) "of" [| (to_conv v).to_jv i |] |> Extension.of_jv

  let from : ('i, 'o) t -> 'a StateField.t -> Extension.t =
   fun v f ->
    Jv.call (to_jv v) "from" [| StateField.to_jv f |] |> Extension.of_jv

  let from' : ('i, 'o) t -> 'a StateField.t -> ('a -> 'i) -> Extension.t =
   fun v f fn ->
    let wrapped_fn x = fn x |> (to_conv v).to_jv in
    Jv.call (to_jv v) "from"
      [| StateField.to_jv f; Jv.callback ~arity:1 wrapped_fn |]
    |> Extension.of_jv
end

module Transaction = struct
  include Transaction

  type selection = Short of { anchor: int; head: int option } | SelectionRange of SelectionRange.t

  type change_spec = { from : int; to_: int option; insert : string option }

  let change_spec_to_jv = function
    | { from; to_; insert } ->
      let o = Jv.obj [||] in
      Jv.set o "from" (Jv.of_int from);
      Jv.set_if_some o "to" (Option.map Jv.of_int to_);
      Jv.set_if_some o "insert" (Option.map Jv.of_string insert);
      o

  let selection_to_jv = function
    | Short { anchor; head } ->
      let o = Jv.obj [||] in
      Jv.set o "anchor" (Jv.of_int anchor);
      Jv.set_if_some o "head" (Option.map Jv.of_int head);
      o
    | SelectionRange r -> SelectionRange.to_jv r

  let effects : t -> StateEffect.t list =
   fun v -> Jv.get (to_jv v) "effects" |> Jv.to_list StateEffect.of_jv

  
  let create ?(effects = []) ?selection ?changes () =
    let o = Jv.obj [||] in
    Jv.set_if_some o "selection" (Option.map selection_to_jv selection);
    Jv.set_if_some o "changes" (Option.map change_spec_to_jv changes);
    Jv.set o "effects" (Jv.of_list StateEffect.to_jv effects);
    of_jv o
end
