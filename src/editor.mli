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

  val create : ?config:Config.t -> unit -> t

  val doc : t -> Text.t
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
    ?root:Brr.El.document ->
    ?dispatch:Jv.t ->
    ?parent:Brr.El.t ->
    unit ->
    opts

  val create : ?opts:opts -> unit -> t
  (** Create a new view *)

  val state : t -> State.t
  (** Current editor state *)

  val set_state : t -> State.t -> unit

  module Update : sig
    type t

    include Jv.CONV with type t := t
  end
end
