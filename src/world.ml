module type Platform = sig
  val id : [ `Posix | `Posix_client | `Js | `Js_client ]
  val year : int
  val base : string
  val default_scheme : string

  val eprint : string -> unit Lwt.t
  val out_of_url : Uri.t -> string -> Url.result Lwt.t
  val in_of_url : Uri.t -> Url.result Lwt.t

  val glo_of_source :
    (string -> string -> string Glo_lib.glo Glo_lib.glom) option
end

module Make(P : Platform) = struct
  type url = Uri.t
  type resource = Glom of string Glo_lib.glo Glo_lib.glom | Glol of Glol_t.glol

  exception GetError of string * string
  exception ParseError of string * string

  let runtime_base = ref ""

  let url = Uri.of_string
  let string = Uri.to_string

  let parse props src =
    match Glo_j.glom_of_string src with
      | (`Assoc al) as json ->
        if List.mem_assoc "glol" al
        then Glol (Glol_lib.glol_of_string src)
        else Glom (Glo_lib.glom_of_json json)
      | json -> Glom (Glo_lib.glom_of_json json)

  let get url = match_lwt P.in_of_url url with
    | Url.Success {Url.uri; Url.props; Url.content} ->
      let uri = Uri.to_string uri in
      begin try Lwt.return (uri, parse props content)
        with Yojson.Json_error _ ->
          match P.glo_of_source with
            | None ->
              let errstr = "No SL parser" in
              Lwt.fail (ParseError (uri,errstr))
            | Some cfn -> Lwt.return (uri, Glom (cfn uri content))
      end
    | Url.Failure (uri,errstr) ->
      Lwt.fail (GetError (Uri.to_string uri, errstr))

  let is_absolute_url url =
    match Uri.scheme url, Uri.host url with
      | Some _, Some _ -> true
      | Some _, None
      | None, Some _
      | None, None -> false

  let resolve_ref base reluri =
    Uri.resolve P.default_scheme
      (Uri.resolve P.default_scheme
         (Uri.of_string !runtime_base) base)
      reluri
end
