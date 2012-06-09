open Js

type headers = (js_string t * js_string t) list

external stdout : js_string t -> (unit -> unit) -> unit = "gloc_stdout"
external stderr : js_string t -> (unit -> unit) -> unit = "gloc_stderr"
external fs_write : js_string t -> js_string t -> (unit -> unit) -> unit = "gloc_fs_write"

external stdin  : (js_string t -> unit) -> unit = "gloc_stdin"
external fs_read : js_string t -> (js_string t -> unit) -> unit = "gloc_fs_read"
external http_get : js_string t ->
  (js_string t -> headers -> js_string t -> unit) ->
    (js_string t -> headers -> js_string t -> unit) -> unit = "gloc_http_get"

module HttpAgent : Url.Agent = struct
  (* TODO: props, errors *)
  let get props url =
    let res, w = Lwt.wait () in
    http_get (string (Uri.to_string url))
      (fun uri hs body -> Lwt.wakeup w
        (Url.Success
           {Url.uri=Uri.of_string (to_string uri);
            Url.props=List.map (fun (h,v) -> (to_string h, to_string v)) hs;
            Url.content=to_string body}))
      (fun uri hs body -> Lwt.wakeup w
        (Url.Failure (Uri.of_string (to_string uri), to_string body)));
    res

  (* TODO: implement *)
  let put props url body =
    Lwt.return (Url.Failure (url, "HTTP output unimplemented"))
end

module FileAgent : Url.Agent = struct
  (* TODO: props, errors *)
  let get props url =
    let res, w = Lwt.wait () in
    (if (Uri.path url)="-" then stdin else fs_read (string (Uri.path url)))
      (fun s -> Lwt.wakeup w
        (Url.Success {Url.uri=url; Url.props=[]; Url.content=to_string s}));
    res

  let put props url body =
    let res, w = Lwt.wait () in
    (if (Uri.path url)="-" then stdout else fs_write (string (Uri.path url)))
      (string body)
      (fun () -> Lwt.wakeup w
        (Url.Success {Url.uri=url; Url.props=[]; Url.content=""}));
    res
end

let id = `Js
let year = (jsnew date_now ())##getFullYear ()
let default_scheme = "file" (* TODO: node vs browser? *)
let base = ""

let eprint s =
  let res, w = Lwt.wait () in
  stderr (string s) (Lwt.wakeup w);
  res
let out_of_url = Url.put ~props:[]
let in_of_url = Url.get ~props:[]

;;
Url.register_scheme "file" (module FileAgent : Url.Agent);
Url.register_scheme "http" (module HttpAgent : Url.Agent)
