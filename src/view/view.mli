open Cm_state

type editor_view
(** The type of {!EditorView.t}, declared ahead of it for the callbacks that
    receive the view. *)

module EditorViewConfig : sig
  type t

  include Jv.CONV with type t := t

  val create :
    ?state:EditorState.t ->
    ?parent:Brr.El.t ->
    ?root:Brr.Document.t ->
    ?dispatch_transactions:(Transaction.t list -> editor_view -> unit) ->
    unit ->
    t

  val undefined : t
end

(** Widgets: elements drawn in the document by a widget decoration. *)
module WidgetType : sig
  type t

  include Jv.CONV with type t := t

  val make : to_dom:(editor_view -> Brr.El.t) -> unit -> t
  (** [make ~to_dom ()] is a widget type whose element is [to_dom view], called
      when the editor needs to draw it. *)
end

module Decoration : sig
  type t

  include Jv.CONV with type t := t

  val mark :
    ?inclusive:bool ->
    ?inclusive_start:bool ->
    ?inclusive_end:bool ->
    ?className:string ->
    ?tagName:string ->
    unit ->
    t

  val widget : ?block:bool -> ?side:int -> WidgetType.t -> t
  (** [widget ?block ?side w] decorates a position with a widget of type [w];
      [side] orders widgets at the same position, [block] puts it on its own
      line. *)

  val none : t RangeSet.t
  val range : from:int -> ?to_:int -> t -> t Range.t
end

module EditorView : sig
  type t = editor_view
  (** Editor view *)

  include Jv.CONV with type t := t

  val create : ?config:EditorViewConfig.t -> unit -> t
  (** Create a new view *)

  val state : t -> EditorState.t
  (** Current editor state *)

  val set_state : t -> EditorState.t -> unit

  module Update : sig
    type t

    val state : t -> EditorState.t

    val doc_changed : t -> bool
    (** Whether this update changed the document. *)

    include Jv.CONV with type t := t
  end

  val dom : t -> Brr.El.t
  val line_wrapping : unit -> Extension.t

  val dispatch : t -> TransactionSpec.t -> unit
  (** [dispatch view spec] makes the transaction [spec] describes and applies it
      to the view. *)

  val set_doc : t -> string -> unit
  (** [set_doc view doc] replaces the whole document with [doc], as one
      transaction. *)

  val request_measure : t -> unit
  (** Asks the view to re-measure on its next layout pass: needed after
      something outside CodeMirror, such as a widget's content, changes size. *)

  type theme = TO of (string * theme) list | TV of string

  val theme : ?dark:bool -> theme -> Extension.t
  val base_theme : theme -> Extension.t
  val decorations : (Decoration.t RangeSet.t, Jv.t) Facet.t
  val update_listener : (Update.t -> unit, Jv.t) Facet.t
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

val showPanel : (Panel.panel_constructor, Jv.t) Facet.t

val line_numbers :
  ?format_number:(int -> EditorState.t -> string) -> unit -> Extension.t
(** The line-number gutter; [format_number n state] renders line [n]. *)
