open Cli
module O = Options

(* session cache *)
let cache = Hashtbl.create 10
let redirect_count = 5

(* Platform properties *)
let id = `Posix
let year = (Unix.gmtime (Unix.time())).Unix.tm_year + 1900
let base = (Unix.getcwd ())^"/"
let default_scheme = "file"

(* TODO: detect redirect loops without redirect_count *)
(* TODO: handle HTTP caching correctly *)
(* TODO: handle HTTP PUT/POST redirects correctly (30x semantics) *)
module HttpAgent : Url.Agent = struct
  let rec request = function
    | (k, abs) when k > redirect_count ->
      Lwt.return (Url.Failure (abs, "too many redirects"))
    | (k, abs) ->
      (try Lwt.return (Url.Success (Hashtbl.find cache abs))
       with Not_found ->
         (try_lwt
            lwt (headers,body) = Cohttpd.Client.get abs in
            let resp = {Url.uri=abs;
                        Url.props=headers;
                        Url.content=body} in
              Lwt.return (Url.Success resp)
          with Cohttpd.Client.Http_error (code,hs,body) ->
            if code < 400 && code >= 300
            then (match (try Some (List.assoc "location" hs)
              with Not_found -> None) with
              | Some loc -> request (k+1, Uri.of_string loc)
              | None -> Lwt.return
                (Url.Failure (abs, "HTTP 3xx redirect without Location")))
            else Lwt.return (Url.Failure (abs, "HTTP "^(string_of_int code)))
      ))

  let get props url = Lwt.bind (request (0, url))
    (fun res -> begin match res with
      | Url.Success resp -> Hashtbl.replace cache url resp
      | Url.Failure _ -> ()
    end; Lwt.return res)

  (* TODO: implement *)
  let put props url body =
    Lwt.return (Url.Failure (url, "HTTP output unimplemented"))
end

module FileAgent : Url.Agent = struct
  (* TODO: props, errors *)
  let get props url =
    lwt content = if (Uri.path url)="-"
      then Io_util.string_of_inchan Lwt_io.stdin
      else Lwt_io.with_file ~mode:Lwt_io.input (Uri.path url)
        Io_util.string_of_inchan
    in Lwt.return (Url.Success {Url.uri=url; Url.props=[]; Url.content})

  (* TODO: props, errors *)
  let put props url body =
    lwt () = if (Uri.path url)=base^"-"
      then Lwt_io.write Lwt_io.stdout body
      else Lwt_io.with_file ~mode:Lwt_io.output (Uri.path url)
        (fun oc -> Lwt_io.write oc body)
    in Lwt.return (Url.Success {Url.uri=url; Url.props=[]; Url.content=""})
end

let out_of_url = Url.put ~props:[]
let in_of_url = Url.get ~props:[]
let eprint = Lwt_io.write Lwt_io.stderr

let glo_of_source =
  let open Gloc_lib in
      Some (fun path src ->
        compile {(O.default_options None) with
          O.stage = O.Compile;
          O.base = Uri.of_string path
        } path src)

;;
Url.register_scheme "file" (module FileAgent : Url.Agent);
Url.register_scheme "http" (module HttpAgent : Url.Agent)
