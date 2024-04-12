type t
(** Editor panels *)

include Jv.CONV with type t := t

val create :
  ?mount:(unit -> unit) ->
  ?update:(View.Update.t -> unit) ->
  ?top:bool ->
  ?pos:int ->
  Brr.El.t ->
  t

module Constructor : sig
  type pc = (View.t -> t) option

  val of_jv : Jv.t -> pc
  val to_jv : pc -> Jv.t
end

val showPanel : (Constructor.pc, Jv.t) Facet.t
