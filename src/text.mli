module Line : sig
    type t
    (** A text line *)
    
    val from : t -> int
    (** Position of the start of the line *)
  
    val to_ : t -> int 
    (** Position at the end of the line before the line break *)

    val number : t -> int
    (** Line's number (1-based) *)
  
    val text : t -> Jstr.t 
    (** Line's text *)
  
    val length : t -> int 
    (** The length of the line *)
  end
  