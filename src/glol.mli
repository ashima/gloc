exception MissingSymbol of string * string
exception MissingMacro of string * string
exception CircularDependency of string list
exception SymbolConflict of string * string * string * string
exception UnknownBehavior of string * string
exception UnknownGloVersion of string * Glo_lib.version
exception BlockedMacro of string * string
val string_of_error : exn -> string

module type World = sig
  type resource = Glom of string Glo_lib.glo Glo_lib.glom | Glol of Glol_t.glol

  exception GetError of (string * string)

  val get : string -> (string * string) Lwt.t

  val resolve_ref : string -> string -> string
  val is_absolute_url : string -> bool
  val parse : string -> resource
end

module Make : functor(W : World) -> sig
  type stream
  type sequence

  val glol_version : int * int * int

  val armor : Glo_lib.meta option -> string * (string * string) list * int
    -> string list -> string -> string
  val nest : (string * string Glo_lib.glo Glo_lib.glom) list -> string Glo_lib.glo Glo_lib.glom

  val li : ?src:string -> Glol_t.li -> stream
  val stream_of_glol : ?src:string -> Glol_t.glol -> stream
  val stream_of_glom : ?src:string -> string Glo_lib.glo Glo_lib.glom -> stream

  val href : ?src:string -> string -> stream
  val define : ?src:string -> string -> string option -> stream
  val concat : ?src:string -> stream -> stream -> stream
  val rename : ?src:string -> (string * string) list -> stream -> stream
  val export : ?src:string -> string list -> stream -> stream

  val glom_of_stream : stream -> string Glo_lib.glo Glo_lib.glom Lwt.t
  val glom_of_sequence : sequence -> string Glo_lib.glo Glo_lib.glom

  (*val index : stream -> store Lwt.t*)
  val satisfy : string list -> stream -> sequence Lwt.t
  (*val union : store -> store -> store*)

  val link : string -> sequence -> string
  (*val ezlink : string -> string list -> stream -> string Lwt.t*)

  val shadow : sequence -> string list
  val meta : sequence -> Glo_lib.meta list
  val linkmap : sequence -> (int * string) list
end
