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

exception Exit of int

let gloc_version = (1,1,0)
let gloc_distributor = "Ashima Arts"

type group = Stage of stage | Format of format
type set = LinkSymbols | LinkDefines | LinkGlobalMacros | LinkRenames | LinkExports
type option = DisableLineControl | Verbose | Dissolve
type filetype = Output | Meta | Input | Base
type iface =
    Group of group
  | Set of set
  | List of iface
  | Filename of filetype
  | Option of option
  | Choice of string list * (string state -> string -> unit)

(* TODO: add per warning check args *)
(* TODO: add partial preprocess (only ambiguous conds with dep macros) *)
(* TODO: add partial preprocess (maximal preprocess without semantic change) *)
(* TODO: make verbose more... verbose *)
let cli = [
  "", List (Filename Input), "source input";
  "", Group (Stage Link), "produce linked SL";
  "-c", Group (Stage Compile), "produce glo and halt; do not link";
  "-E", Group (Stage Preprocess), "preprocess and halt; do not parse SL";
  "-e", Group (Stage ParsePP), "parse preprocessor and halt; do not preprocess";
  "--source", Group (Stage Contents),
  "strip the glo format and return the contained source";
  "--glolli", Group (Stage Glolli),
  "generate the glolli expression from this command";
  "--json", Group (Format JSON), "produce JSON (default)";
  "--xml", Group (Format XML), "produce XML";
  "-u", List (Set LinkSymbols), "required symbol (default ['main'])";
  "--define", List (Set LinkDefines), "define a macro unit";
  "-D", List (Set LinkGlobalMacros), "define a global macro";
  "-o", Filename Output, "output file";
  "--accuracy", Choice (["best";"preprocess"],
                        fun exec_state s ->
                          exec_state.accuracy := (Language.accuracy_of_string s)),
  "output accuracy";
  "--line", Option DisableLineControl, "disregard incoming line directives";
  "-x", Choice (["webgl"],
                fun exec_state s ->
                  exec_state.inlang :=
                    (Language.with_dialect s !(exec_state.inlang))),
  "source language";
  "-t", Choice (["webgl"],
                fun exec_state s ->
                  exec_state.outlang :=
                    (Language.with_dialect s !(exec_state.outlang))),
  "target language";
  "--dissolve", Option Dissolve, "dissolve declarations"; (* TODO: fix bugs *)
  "--rename", List (Set LinkRenames), "rename identifiers";
  "--export", List (Set LinkExports), "export identifiers";
  "-v", Option Verbose, "verbose";
  "--meta", Filename Meta, "prototypical glo file to use for metadata";
  "--base", Filename Base, "base URI";
]

let arg_of_group cli el =
  let rec search = function
    | (arg,Group x,_)::_ when x=el -> arg
    | _::r -> search r
    | [] -> "<default>"
  in search cli

let set_of_group state cli =
  let set_stage stage = fun () ->
    if !(state.stage)=Link
    then state.stage := stage
    else raise (CompilerError (Command,
                               [IncompatibleArguments
                                   (arg_of_group cli (Stage !(state.stage)),
                                    arg_of_group cli (Stage stage))])) in
  let set_format fmt = fun () ->
    if !(state.format)=JSON
    then state.format := fmt
    else raise (CompilerError (Command,
                               [IncompatibleArguments
                                   (arg_of_group cli (Format !(state.format)),
                                    arg_of_group cli (Format fmt))]))
  in function
    | Stage s -> set_stage s
    | Format s -> set_format s

let string_of_version (maj,min,rev) = Printf.sprintf "%d.%d.%d" maj min rev

let string_of_platform_id = function
  | `Posix -> "posix"
  | `Js -> "js"

module Make(P : World.Platform) = struct

  module World = World.Make(P)
  module Glol = Glol.Make(World)

  let usage_msg = Printf.sprintf "gloc(%s) version %s (%s)"
    (string_of_platform_id P.id)
    (string_of_version gloc_version)
    gloc_distributor

  let empty_meta =
    { copyright=(P.year,("",""));
      author=[]; license=None; library=None; version=None; build=None }

  let meta_of_url url =
    lwt s = P.in_of_url url in
    match s with
      | Url.Success {Url.content} -> Lwt.return (glo_of_string content).meta
      | Url.Failure (final, errstr) ->
        Lwt.fail (MetaPrototypeFailure ("<"^(Uri.to_string final)^">: "^errstr))

  let spec_of_iface exec_state cli = function
    | Group g -> A.Unit (set_of_group exec_state cli g)
    | List (Set LinkSymbols) ->
      A.String (fun u -> exec_state.symbols := u::!(exec_state.symbols))
    | List (Set LinkDefines) ->
      A.String (fun m -> exec_state.inputs := (Def m)::!(exec_state.inputs))
    | List (Set LinkGlobalMacros) ->
      A.String (fun m -> exec_state.prologue := m::!(exec_state.prologue))
    | List (Set LinkRenames) ->
      A.String (fun m -> exec_state.renames := m::!(exec_state.renames))
    | List (Set LinkExports) ->
      A.String (fun m -> exec_state.exports := m::!(exec_state.exports))
    | Filename Output ->
      A.String (fun m -> exec_state.output := m)
    | List (Filename Input) ->
      A.String (fun m -> exec_state.inputs := (Stream m)::!(exec_state.inputs))
    | Filename Meta ->
      A.String (fun m -> exec_state.metadata := Some m)
    | Filename Base ->
      A.String (fun m -> exec_state.base := m)
    | Option DisableLineControl -> A.Clear exec_state.linectrl
    | Option Dissolve -> A.Set exec_state.dissolve
    | Option Verbose -> A.Set exec_state.verbose
    | Choice (syms, setfn) -> A.Symbol (syms, setfn exec_state)
    | Set _ -> raise (Failure "no untagged set symbols") (* TODO: real exn *)
    | List _ -> raise (Failure "other lists unimplemented") (* TODO: real exn *)
    | Filename Input -> raise (Failure "input files always list") (* TODO: real exn *)

  let arg_of_cli exec_state cli = List.fold_right
    (fun (flag, iface, descr) (argl, anon) ->
      if flag="" then (argl, match spec_of_iface exec_state cli iface with
        | A.String sfn -> sfn
        | _ -> fun _ -> ()
      ) else ((flag, spec_of_iface exec_state cli iface, descr)::argl, anon)
    ) cli ([], fun _ -> ())

  let print_errors linectrl errors =
    Lwt_list.iter_s (fun e -> P.eprint ((string_of_error linectrl e)^"\n"))
      (List.rev errors)

  let compiler_error exec_state k errs =
    let code = exit_code_of_component k in
    lwt () = print_errors !(exec_state.linectrl) errs in
    lwt () = P.eprint ("Fatal: "^(string_of_component_error k)
                       ^" ("^(string_of_int code)^")\n") in
    Lwt.fail (Exit code)

  let rec write_glom exec_state path = function
    | Source s -> write_glom exec_state path (make_glo exec_state path s)
    | Other o ->
      if !(exec_state.verbose)
      then Yojson.Safe.pretty_to_string ~std:true o
      else Yojson.Safe.to_string ~std:true o
    | Leaf glo ->
      let s = string_of_glo glo in
      (if !(exec_state.verbose)
       then Yojson.Safe.prettify ~std:true s else s)^"\n"
    | Glom glom ->
      let json = json_of_glom (Glom glom) in
      (if !(exec_state.verbose)
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

  let write_glopp exec_state path prologue glom =
    let ppexpr = parse !(exec_state.inlang) (source_of_glom path glom) in
    (string_of_ppexpr start_loc
       (match !(exec_state.stage) with
         | ParsePP -> ppexpr
         | Preprocess ->
           check_pp_divergence (preprocess !(exec_state.outlang) ppexpr)
         | s -> (* TODO: stage? error message? tests? *)
           raise (CompilerError (PPDiverge, [GloppStageError s]))
       ))^"\n"

  let write_glol exec_state uri glol =
    let s = Glol_lib.string_of_glol glol in
    (if !(exec_state.verbose)
     then Yojson.Safe.prettify ~std:true s
     else s)^"\n"

  let streamp = function Stream _ -> true | _ -> false
  let stream_inputp il = List.exists streamp il
  let glom_of_input = function Stream f -> f | Def f -> f

  let prologue exec_state = List.fold_left
    (fun s ds -> (make_define_line ds)^s) "" !(exec_state.prologue)

  (* TODO: Check hyperlink freshness *)
  let refresh glom = glom

  (* Generate a glolli expr *)
  let gen_glolli exec_state =
    let glolli_of_input = function
      | Stream i -> if "-"=i then `A "file:-" else `A i
      | Def ds -> `Define (split_define_string ds)
    in
    let inputs = match !(exec_state.inputs) with
      | [] -> `A "file:-"
      | [i] -> glolli_of_input i
      | il -> `Concat (List.map glolli_of_input il)
    in
    let rename = match !(exec_state.renames) with
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
    let export = match !(exec_state.exports) with
      | [] -> rename
      | exports -> `Export (exports, rename)
    in export

  let link prologue required glol =
    try_lwt lwt seq = Glol.satisfy required
    (Glol.stream_of_glol ~src:"[gloc]" glol)
    in Lwt.return (Glol.link prologue seq)
    with e -> raise (CompilerError (Linker,[e]))

  let nglom_of_glol exec_state uri glol =
    lwt glom = Glol.glom_of_stream (Glol.stream_of_glol glol) in
    let scheme = match Uri.scheme uri with
      | Some s -> s
      | None -> "file" in
    if "file"=scheme
    then Lwt.return (!(exec_state.output),glom)
    else Lwt.return ("<"^(Uri.to_string uri)^">",glom)

  let gloc exec_state =
    lwt metadata = match !(exec_state.metadata) with
      | None -> Lwt.return (ref None)
      | Some s -> lwt meta = meta_of_url (Uri.of_string s) in Lwt.return (ref meta)
    in
    let exec_state = {exec_state with metadata} in
    let req_sym = List.rev !(exec_state.symbols) in

    let glolli = gen_glolli exec_state in
    let base = !(exec_state.base) in
    let glol = {Glol_lib.glol=Glol.glol_version;
                Glol_lib.base=(if base = "" then None else Some base);
                Glol_lib.transmit=None;
                Glol_lib.li=Some glolli;
                Glol_lib.cache=[];
                Glol_lib.glom=None} in

    let base = Uri.resolve P.default_scheme
      (Uri.of_string P.base) (Uri.of_string base) in

    let () = World.runtime_base := (Uri.to_string base) in

    let uri = Uri.resolve P.default_scheme
      base (Uri.of_string !(exec_state.output)) in

    try_lwt begin
      lwt result = match !(exec_state.stage) with
        | Glolli -> P.out_of_url uri
          (match !(exec_state.format) with
            | JSON -> write_glol exec_state uri glol
            | XML -> "" (* TODO: do *) (*Glol_xml.xml_of_glol ~pretty:true glol*)
          )
        | Contents ->
          lwt (path,glom) = nglom_of_glol exec_state uri glol in
            P.out_of_url uri (source_of_glom path glom)
        | Link ->
          let prologue = prologue exec_state in
          lwt src = link prologue req_sym glol in
            P.out_of_url uri
              (if !(exec_state.accuracy)=Language.Preprocess
               then string_of_ppexpr start_loc
                  (check_pp_divergence
                     (preprocess !(exec_state.inlang)
                        (parse !(exec_state.inlang) src)))
               else src)
        | Compile -> (* TODO: consolidate glo? *)
          lwt (path,glom) = nglom_of_glol exec_state uri glol in
            begin match !(exec_state.format) with
              | JSON -> P.out_of_url uri
                (write_glom exec_state path (minimize_glom glom))
              | XML -> P.out_of_url uri
                (Glo_xml.xml_of_glom ~pretty:true glom)
            end
        | Preprocess | ParsePP ->
          lwt (path,glom) = nglom_of_glol exec_state uri glol in
          let prologue = prologue exec_state in
            P.out_of_url uri (write_glopp exec_state path prologue glom)
      in match result with
        | Url.Success _ -> Lwt.return ()
        | Url.Failure (final,errstr) ->
          Lwt.fail (CompilerError (WriteOut,
                                   [OutputFailure (Uri.to_string final,
                                                   errstr)]))
    end with CompilerError (k,errs) -> compiler_error exec_state k errs
end
