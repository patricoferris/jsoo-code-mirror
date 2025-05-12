type t
(** Extensions compartments for the editor *)

include Jv.CONV with type t := t

val make : unit -> t
val of' : t -> Extension.t list -> Extension.t
val reconfigure : t -> Extension.t list -> Editor.View.Transaction.t
