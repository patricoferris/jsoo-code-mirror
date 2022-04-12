open Code_mirror

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

  val state : t -> Editor.State.t
  (** The editor state that the completion happens in. *)

  val pos : t -> int
  (** The position at which the completion is happening. *)

  val explicit : t -> bool
  (** Indicates whether completion was activated explicitly, or implicitly by
    typing. The usual way to respond to this is to only return completions when
    either there is part of a completable entity before the cursor, or explicit
    is true. *)

  val token_before : t -> string list -> Jv.t option

  val match_before : t -> RegExp.t -> Jv.t option

  val aborted : t -> bool
  (** Yields true when the query has been aborted. Can be useful in
    asynchronous queries to avoid doing work that will be ignored. *)
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

module Source : sig
  type t
  (** A Completion source *)

  include Jv.CONV with type t := t

  val create : (Context.t -> Result.t option Fut.t) -> t

  val from_list : Completion.t list -> t
end

type config

val config :
  ?activate_on_typing:bool ->
  ?override:Source.t list ->
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
