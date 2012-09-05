(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

(*open Printf*)
module A = Arg

open Pp_lib
open Pp
open Esslpp_lex
open Glo_lib
open Gloc_lib
open Options

exception Exit of int

module Make(P : World.Platform) = struct

  module World = World.Make(P)
  module Glol = Glol.Make(World)

  let empty_meta =
    { copyright=(P.year,("",""));
      author=[]; license=None; library=None; version=None; build=None }

  let meta_of_url url =
    lwt s = P.in_of_url url in
    match s with
      | Url.Success {Url.content} -> Lwt.return (glo_of_string content).meta
      | Url.Failure (final, errstr) ->
        Lwt.fail (MetaPrototypeFailure ("<"^(Uri.to_string final)^">: "^errstr))

  let print_errors linectrl errors =
    Lwt_list.iter_s (fun e -> P.eprint ((string_of_error linectrl e)^"\n"))
      (List.rev errors)

  let compiler_error options k errs =
    let code = exit_code_of_component k in
    lwt () = print_errors options.linectrl errs in
    lwt () = P.eprint ("Fatal: "^(string_of_component_error k)
                       ^" ("^(string_of_int code)^")\n") in
    Lwt.fail (Exit code)

  let rec write_glom options path = function
    | Source s -> write_glom options path (make_glo options path s)
    | Other o ->
      if options.verbose
      then Yojson.Safe.pretty_to_string ~std:true o
      else Yojson.Safe.to_string ~std:true o
    | Leaf glo ->
      let s = string_of_glo glo in
      (if options.verbose
       then Yojson.Safe.prettify ~std:true s else s)^"\n"
    | Glom glom ->
      let json = json_of_glom (Glom glom) in
      (if options.verbose
       then Yojson.Safe.pretty_to_string ~std:true json
       else Yojson.Safe.to_string ~std:true json)^"\n"

  let rec source_of_glom path = function
    | Source s -> begin match glom_of_string s with Source s -> s
        | glom -> source_of_glom path glom end
    | Leaf glo -> if (Array.length glo.units)=1
      then Glol.armor glo.meta (path,glo.linkmap,0) []
        glo.units.(0).source
      else raise (CompilerError (PPDiverge, [MultiUnitGloPPOnly path]))
    | Glom ((n,glom)::[]) -> source_of_glom (path^"/"^n) glom
    | Glom _ -> raise (CompilerError (PPDiverge, [GlomPPOnly path]))
    | Other o -> raise (Failure "cannot get source of unknown json") (* TODO *)

  let write_glopp options path prologue glom =
    let ppexpr = parse options.inlang (source_of_glom path glom) in
    (string_of_ppexpr start_loc
       (match options.stage with
         | ParsePP -> ppexpr
         | Preprocess ->
           check_pp_divergence (preprocess options.outlang ppexpr)
         | s -> (* TODO: stage? error message? tests? *)
           raise (CompilerError (PPDiverge, [GloppStageError s]))
       ))^"\n"

  let write_glol options uri glol =
    let s = Glol_lib.string_of_glol glol in
    (if options.verbose
     then Yojson.Safe.prettify ~std:true s
     else s)^"\n"

  let streamp = function Stream _ -> true | _ -> false
  let stream_inputp il = List.exists streamp il
  let glom_of_input = function Stream f -> Uri.to_string f | Def f -> f | STDIN -> "-"

  let prologue options = List.fold_left
    (fun s ds -> (make_define_line ds)^s) "" options.prologue

  (* TODO: Check hyperlink freshness *)
  let refresh glom = glom

  (* Generate a glolli expr *)
  let gen_glolli options =
    let glolli_of_input = function
      | STDIN -> `A "file:-" 
      | Stream path -> `A (Uri.to_string path)
      | Def ds -> `Define (split_define_string ds)
    in
    let inputs = match options.inputs with
      | [] -> `A "file:-"
      | [i] -> glolli_of_input i
      | il -> `Concat (List.map glolli_of_input il)
    in
    let rename = match options.renames with
      | [] -> inputs
      | renames -> `Rename
        (List.map
           (fun r -> match split_define_string r with
             | src, None ->
               raise (CompilerError (Command,[InvalidRenameString src]))
             | src, Some tgt -> (src, tgt)
           ) renames,
         inputs)
    in
    let export = match options.exports with
      | [] -> rename
      | exports -> `Export (exports, rename)
    in export

  let link prologue required glol =
    try_lwt lwt seq = Glol.satisfy required
    (Glol.stream_of_glol ~src:"[gloc]" glol)
    in Lwt.return (Glol.link prologue seq)
    with e -> raise (CompilerError (Linker,[e]))

  let nglom_of_glol options uri glol =
    lwt glom = Glol.glom_of_stream (Glol.stream_of_glol glol) in
    let scheme = match Uri.scheme uri with
      | Some s -> s
      | None -> "file" in
    let path = match options.output with
      | STDOUT -> "{STDOUT}"
      | Path path -> Uri.to_string path in
    if "file"=scheme
    then Lwt.return (path,glom)
    else Lwt.return ("<"^(Uri.to_string uri)^">",glom)

  let gloc options =
    lwt metadata = match options.metadata with
      | None -> Lwt.return None
      | Some s -> lwt meta = meta_of_url (Uri.of_string s) in Lwt.return meta
    in
    let options = {options with metadata} in
    let req_sym = List.rev options.symbols in

    let glolli = gen_glolli options in
    let base = options.base in
    let glol = {Glol_lib.glol=Glol.glol_version;
                Glol_lib.base=(if base = Uri.of_string "" then None else Some (Uri.to_string base));
                Glol_lib.transmit=None;
                Glol_lib.li=Some glolli;
                Glol_lib.cache=[];
                Glol_lib.glom=None} in

    let base = Uri.resolve P.default_scheme
      (Uri.of_string P.base) base in

    let () = World.runtime_base := (Uri.to_string base) in

    let uri = Uri.resolve P.default_scheme
      base (let Path actual = options.output in actual) in (* TODO: Handle STDOUT *)

    try_lwt begin
      lwt result = match options.stage with
        | Glolli -> P.out_of_url uri
          (match options.format with
            | JSON -> write_glol options uri glol
            | XML -> "" (* TODO: do *) (*Glol_xml.xml_of_glol ~pretty:true glol*)
          )
        | Contents ->
          lwt (path,glom) = nglom_of_glol options uri glol in 
            P.out_of_url uri (source_of_glom path glom)
        | Link ->
          let prologue = prologue options in
          lwt src = link prologue req_sym glol in
            P.out_of_url uri
              (if options.accuracy=Language.Preprocess
               then string_of_ppexpr start_loc
                  (check_pp_divergence
                     (preprocess options.inlang
                        (parse options.inlang src)))
               else src)
        | Compile -> (* TODO: consolidate glo? *)
          lwt (path,glom) = nglom_of_glol options uri glol in 
            begin match options.format with
              | JSON -> P.out_of_url uri
                (write_glom options path (minimize_glom glom))
              | XML -> P.out_of_url uri
                (Glo_xml.xml_of_glom ~pretty:true glom)
            end
        | Preprocess | ParsePP ->
          lwt (path,glom) = nglom_of_glol options uri glol in 
          let prologue = prologue options in
            P.out_of_url uri (write_glopp options path prologue glom)
      in match result with
        | Url.Success _ -> Lwt.return ()
        | Url.Failure (final,errstr) ->
          Lwt.fail (CompilerError (WriteOut,
                                   [OutputFailure (Uri.to_string final,
                                                   errstr)]))
    end with CompilerError (k,errs) -> compiler_error options k errs
end
