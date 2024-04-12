type 'a conv = {
  to_jv : 'a -> Jv.t;
  of_jv : Jv.t -> 'a;
}

module Effect : sig
  type t
  
  type 'a ty 
  val define : ('a -> Jv.t) -> (Jv.t -> 'a) -> 'a ty

  val is : t -> 'a ty -> bool

  val value : t -> 'a ty -> 'a option

  val of_ : 'a ty -> 'a -> t
end


module State : sig

  type t

  include Jv.CONV with type t := t


  module Transaction : sig
    type t

    include (Jv.CONV with type t := t)

    val effects : t -> Effect.t list

    val create : ?effects:Effect.t list -> unit -> t
  end

  module Field 
  : sig
    type 'a field

    val extension : 'a field -> Extension.t

    val define : 'a. ?compare:('a -> 'a -> bool) -> ?provide:('a field -> Extension.t) -> ('a -> Jv.t) -> (Jv.t -> 'a) -> create:(t -> 'a) -> update:('a -> Transaction.t -> 'a) -> 'a field
    
  end 

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

  module Facet : sig
    type ('input, 'output) t

    val to_jv : ('input, 'output) t -> Jv.t

    val of_ : ('input, 'output) t -> 'input -> Extension.t

    val from : ('input, 'output) t -> 'a Field.field -> Extension.t

    val from' : ('input, 'output) t -> 'a Field.field -> ('a -> 'input) -> Extension.t

    val create : 'input conv -> Jv.t -> ('input, 'output) t
  end

  val create : ?config:Config.t -> unit -> t
  val field : t -> 'a Field.field -> 'a
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
  val update_listener : unit -> (Update.t -> unit, Jv.t) State.Facet.t
  val line_wrapping : unit -> Extension.t
  val dispatch : t -> State.Transaction.t -> unit
end

module Keymap : sig
  type t

  include Jv.CONV with type t := t

  type command = View.t -> bool

  val create : ?key:string -> ?run:command -> unit -> t

end 

val keymap : (Keymap.t, Jv.t) State.Facet.t