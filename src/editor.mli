module Selection : sig
  type t

  include Jv.CONV with type t := t

  module Range : sig
    type t

    include Jv.CONV with type t := t

    val head : t -> int
    (** The head of the range *)

    val from : t -> int
    (** The lower boundary of the range *)

    val to' : t -> int
    (** The upper boundary of the range *)

    val anchor : t -> int
    (** The anchor of the range, the one that does not move when extended *)
  end

  val main : t -> Range.t
  (** Get the primary selection range *)
end

module State : sig
  type t

  include Jv.CONV with type t := t

  module Config : sig
    type t

    (* TODO: Add selection *)
    val create :
      ?doc:Jstr.t ->
      ?selection:Jv.t ->
      ?extensions:Extension.t array ->
      unit ->
      t
  end

  module type Facet = sig
    type t

    include Jv.CONV with type t := t

    type input
    type output

    val of_ : t -> input -> Extension.t
  end

  module FacetMaker : functor
    (I : sig
       type t

       include Jv.CONV with type t := t
     end)
    -> Facet with type input = I.t

  type ('i, 'o) facet =
    | Facet :
        (module Facet with type input = 'i and type output = 'o and type t = 'a)
        * 'a
        -> ('i, 'o) facet

  val create : ?config:Config.t -> unit -> t
  (** Create a new state *)

  val doc : t -> Text.t
  (** Get the current document *)

  val selection : t -> Selection.t
  (** Get the current selection *)
end

module View : sig
  type t
  (** Editor view *)

  include Jv.CONV with type t := t

  type opts
  (** Configurable options for the editor view *)

  (* TODO: Dispatch function *)
  val opts :
    ?state:State.t ->
    ?parent:Brr.El.t ->
    ?root:Brr.El.document ->
    ?dispatch:Jv.t ->
    unit ->
    opts

  val create : ?opts:opts -> unit -> t
  (** Create a new view *)

  val state : t -> State.t
  (** Current editor state *)

  val set_state : t -> State.t -> unit

  module Update : sig
    type t

    val state : t -> State.t

    include Jv.CONV with type t := t
  end

  module Plugin : sig
    type view := t
    type t
    (** A {{: https://codemirror.net/docs/ref/#view.ViewPlugin} view plugin} *)

    val v : (view -> unit) -> t
    (** Create a new view plugin *)

    val to_extension : t -> Extension.t
    (** Coerce the plugin to an {! Extension.t} *)
  end

  val dom : t -> Brr.El.t
  val update_listener : unit -> (Update.t -> unit, Jv.t) State.facet
  val line_wrapping : unit -> Extension.t
end
