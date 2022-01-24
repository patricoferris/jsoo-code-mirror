val autocomplete : Jv.t
(** Global autocomplete value *)

module RegExp = RegExp

module Completion : sig
  type t

  include Jv.CONV with type t := t

  val create :
    label:string ->
    ?detail:string ->
    ?info:string -> 
    ?apply:t ->
    ?type_:string ->
    ?boost:int ->
    unit -> t
end

module Context : sig
  type t
  (** Completion context *)

  include Jv.CONV with type t := t

  val token_before : t -> string list -> Jv.t option

  val match_before : t -> RegExp.t -> Jv.t option
end

module Result : sig
  type t
  (** Completion result *)

  include Jv.CONV with type t := t

  val create :
    from:int ->
    ?to_:int ->
    options:Completion.t list ->
    ?span:RegExp.t ->
    ?filter:bool ->
    unit -> t
  (** Creating a new completion result (see {{: https://codemirror.net/6/docs/ref/#autocomplete.CompletionResult} the docs}).*)
end

type source = Context.t -> Result.t option Fut.t
(** A completion source *)


type config

val config : 
  ?activate_on_typing:bool -> 
  ?override:source list ->
  ?max_rendered_options:int ->
  ?default_key_map:bool ->
  ?above_cursor:bool ->
  ?option_class:Jv.t ->
  ?icons:bool ->
  ?add_to_options:Jv.t ->
  unit ->
  config
  (** Configuration options for your autocompleter, see {{: https://codemirror.net/6/docs/ref/#autocomplete.autocompletion^config} the online docs}.*)

val create :
  ?config:config -> unit ->
  Code_mirror.Extension.t
  (** Autocompleter *)
