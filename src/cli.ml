(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: Tikhon Jelvis
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)
open Gloc_lib
open Semver
module O = Options

let string_of_platform_id = function
  | `Posix -> "posix"
  | `Posix_client -> "posix client"
  | `Js -> "js"
  | `Js_client -> "js client"

let usage_msg platform_id = Printf.sprintf "gloc(%s) version %s (%s)"
  (string_of_platform_id platform_id)
  (print_version O.gloc_version)
  O.gloc_distributor

type input = Stream of string | Def of string

type 'a state = {
  stage    : O.stage ref;
  format   : O.format ref;
  verbose  : bool ref;
  dissolve : bool ref;
  linectrl : bool ref;
  metadata : 'a option ref;
  renames  : string list ref;
  exports  : string list ref;
  symbols  : string list ref;
  base     : string ref;
  output   : string ref;
  inputs   : input list ref;
  prologue : string list ref;
  inlang   : Language.language ref;
  outlang  : Language.language ref;
  accuracy : Language.accuracy ref;
}

let new_exec_state meta = {
  stage    = ref O.Link;
  format   = ref O.JSON;
  verbose  = ref false;
  dissolve = ref false;
  linectrl = ref true;
  metadata = ref meta;
  renames  = ref [];
  exports  = ref [];
  symbols  = ref [];
  base     = ref "";
  output   = ref "-";
  inputs   = ref [];
  prologue = ref [];
  inlang   = ref O.default_lang;
  outlang  = ref O.default_lang;
  accuracy = ref Language.Best;
}

let convert_input = (function
  | Stream "-"  -> O.STDIN
  | Stream path -> O.Stream (Uri.of_string path)
  | Def x       -> O.Def x)

let freeze exec_state = {
  O.stage    = !(exec_state.stage);
  O.format   = !(exec_state.format);
  O.verbose  = !(exec_state.verbose);
  O.dissolve = !(exec_state.dissolve);
  O.linectrl = !(exec_state.linectrl);
  O.metadata = !(exec_state.metadata);
  O.renames  = !(exec_state.renames);
  O.exports  = !(exec_state.exports);
  O.symbols  = !(exec_state.symbols);
  O.base     = Uri.of_string !(exec_state.base);
  O.output   = O.output_of_string !(exec_state.output);
  O.inputs   = List.map convert_input !(exec_state.inputs);
  O.prologue = !(exec_state.prologue);
  O.inlang   = !(exec_state.inlang);
  O.outlang  = !(exec_state.outlang);
  O.accuracy = !(exec_state.accuracy);
}

type group = Stage of O.stage | Format of O.format
type set = LinkSymbols | LinkDefines | LinkGlobalMacros | LinkRenames | LinkExports
type option_flag = DisableLineControl | Verbose | Dissolve
type filetype = Output | Meta | Input | Base
type iface =
    Group of group
  | Set of set
  | List of iface
  | Filename of filetype
  | Option of option_flag
  | Choice of string list * (string state -> string -> unit)

(* TODO: add per warning check args *)
(* TODO: add partial preprocess (only ambiguous conds with dep macros) *)
(* TODO: add partial preprocess (maximal preprocess without semantic change) *)
(* TODO: make verbose more... verbose *)
let cli_spec = [
  "", List (Filename Input), "source input";
  "", Group (Stage O.Link), "produce linked SL";
  "-c", Group (Stage O.Compile), "produce glo and halt; do not link";
  "-E", Group (Stage O.Preprocess), "preprocess and halt; do not parse SL";
  "-e", Group (Stage O.ParsePP), "parse preprocessor and halt; do not preprocess";
  "--source", Group (Stage O.Contents),
  "strip the glo format and return the contained source";
  "--glolli", Group (Stage O.Glolli),
  "generate the glolli expression from this command";
  "--json", Group (Format O.JSON), "produce JSON (default)";
  "--xml", Group (Format O.XML), "produce XML";
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
    if !(state.stage) = O.Link
    then state.stage := stage
    else raise (CompilerError (Command,
                               [IncompatibleArguments
                                   (arg_of_group cli (Stage !(state.stage)),
                                    arg_of_group cli (Stage stage))])) in
  let set_format fmt = fun () ->
    if !(state.format)=O.JSON
    then state.format := fmt
    else raise (CompilerError (Command,
                               [IncompatibleArguments
                                   (arg_of_group cli (Format !(state.format)),
                                    arg_of_group cli (Format fmt))]))
  in function
    | Stage s -> set_stage s
    | Format s -> set_format s

let spec_of_iface exec_state cli = function
  | Group g -> Arg.Unit (set_of_group exec_state cli g)
  | List (Set LinkSymbols) ->
    Arg.String (fun u -> exec_state.symbols := u::!(exec_state.symbols))
  | List (Set LinkDefines) ->
    Arg.String (fun m -> exec_state.inputs := (Def m)::!(exec_state.inputs))
  | List (Set LinkGlobalMacros) ->
    Arg.String (fun m -> exec_state.prologue := m::!(exec_state.prologue))
  | List (Set LinkRenames) ->
    Arg.String (fun m -> exec_state.renames := m::!(exec_state.renames))
  | List (Set LinkExports) ->
    Arg.String (fun m -> exec_state.exports := m::!(exec_state.exports))
  | Filename Output ->
    Arg.String (fun m -> exec_state.output := m)
  | List (Filename Input) ->
    Arg.String (fun m -> exec_state.inputs := (Stream m)::!(exec_state.inputs))
  | Filename Meta ->
    Arg.String (fun m -> exec_state.metadata := Some m)
  | Filename Base ->
    Arg.String (fun m -> exec_state.base := m)
  | Option DisableLineControl -> Arg.Clear exec_state.linectrl
  | Option Dissolve -> Arg.Set exec_state.dissolve
  | Option Verbose -> Arg.Set exec_state.verbose
  | Choice (syms, setfn) -> Arg.Symbol (syms, setfn exec_state)
  | Set _ -> raise (Failure "no untagged set symbols") (* TODO: real exn *)
  | List _ -> raise (Failure "other lists unimplemented") (* TODO: real exn *)
  | Filename Input -> raise (Failure "input files always list") (* TODO: real exn *)

let arg_of_cli exec_state cli = List.fold_right
  (fun (flag, iface, descr) (argl, anon) ->
    if flag="" then (argl, match spec_of_iface exec_state cli iface with
      | Arg.String sfn -> sfn
      | _ -> fun _ -> ()
    ) else ((flag, spec_of_iface exec_state cli iface, descr)::argl, anon)
  ) cli ([], fun _ -> ())

(* This throws an exception on -help or malformed input. *)
let options_of_args platform_id args =
  let exec_state = new_exec_state None in
  let (specs, anon) = arg_of_cli exec_state cli_spec in
  let () = Arg.parse_argv ~current:(ref 0) args specs anon (usage_msg platform_id) in
  freeze exec_state

(* This takes an alist like the one used for the URI query and turns
   it into a command-line command. *)
let command_of_alist alist =
  let to_cmd = function
    | ""                         -> ""
    | s when String.length s < 2 -> "-"^s
    | s                          -> "--"^s in
  let ordered_alist = List.append (List.filter (fun (x,y) -> x = "") alist)
                                  (List.filter (fun (x,y) -> x <> "") alist) in
  let process = function
    | "", ""      -> ""
    | name, value -> to_cmd name ^ " " ^ String.escaped value ^ " " in
  "gloc" ^ List.fold_left (^) "" (List.map process ordered_alist)

let command_of_options options = command_of_alist (alist_of_options options)
