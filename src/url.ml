type props = (string * string) list
type response = {
  uri: Uri.t;
  props: props;
  content: string;
}

type result = Success of response | Failure of Uri.t * string

exception UnknownScheme of string

module type Agent = sig
    val get : props -> Uri.t -> result Lwt.t
    val put : props -> Uri.t -> string -> result Lwt.t
end

let schemes = Hashtbl.create 2
let register_scheme = Hashtbl.replace schemes 

let module_of_uri uri = match Uri.scheme uri with
  | Some scheme -> begin try Hashtbl.find schemes scheme
    with Not_found -> raise (UnknownScheme scheme)
  end
  | None -> raise (UnknownScheme "<empty>") (* TODO: exn *)

let get ?(props=[]) uri =
  let module A = (val (module_of_uri uri) : Agent) in A.get props uri

let put ?(props=[]) uri content =
  let module A = (val (module_of_uri uri) : Agent) in A.put props uri content
      
