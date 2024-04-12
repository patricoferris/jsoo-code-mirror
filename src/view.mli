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

val dom : t -> Brr.El.t
val update_listener : unit -> (Update.t -> unit, Jv.t) Facet.t
val line_wrapping : unit -> Extension.t
val dispatch : t -> Transaction.t -> unit
