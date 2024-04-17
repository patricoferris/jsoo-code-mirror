module EditorViewConfig : sig
  type t

  include Jv.CONV with type t := t

  val create :
    ?state:State.EditorState.t ->
    ?parent:Brr.El.t ->
    ?root:Brr.Document.t ->
    ?dispatch_transactions:
      (State.Transaction.t list -> Types.View.EditorView.t -> unit) ->
    unit ->
    t

  val undefined : t
end

module EditorView : sig
  type t
  (** Editor view *)

  include Jv.CONV with type t := t

  val create : ?config:EditorViewConfig.t -> unit -> t
  (** Create a new view *)

  val state : t -> State.EditorState.t
  (** Current editor state *)

  val set_state : t -> State.EditorState.t -> unit

  module Update : sig
    type t

    val state : t -> State.EditorState.t

    include Jv.CONV with type t := t
  end

  val dom : t -> Brr.El.t
  val update_listener : unit -> (Update.t -> unit, Jv.t) State.Facet.t
  val line_wrapping : unit -> Extension.t
  val dispatch : t -> State.Transaction.t -> unit
end

module Panel : sig
  type t

  include Jv.CONV with type t := t

  val create :
    ?mount:(unit -> unit) ->
    ?update:(EditorView.Update.t -> unit) ->
    ?top:bool ->
    ?destroy:(unit -> unit) ->
    Brr.El.t ->
    t

  type panel_constructor = (EditorView.t -> t) option
end

val showPanel : (Panel.panel_constructor, Jv.t) State.Facet.t
