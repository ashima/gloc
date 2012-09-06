(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: Tikhon Jelvis
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)
open List
open Semver

type stage = Glolli | Contents | ParsePP | Preprocess | Compile | Link
type format = XML | JSON

type input = STDIN | Stream of Uri.t | Def of string * string
type output = STDOUT | Path of Uri.t

type 'a options = {
  stage    : stage;
  format   : format;
  verbose  : bool;
  dissolve : bool;
  linectrl : bool;
  metadata : 'a;
  renames  : string list;               (* symbol * symbol list *)
  exports  : string list;               (* symbol list *)
  symbols  : string list;               (* symbol list *)
  base     : Uri.t;
  output   : output;
  inputs   : input list;
  prologue : string list;
  inlang   : Language.language;
  outlang  : Language.language;
  accuracy : Language.accuracy;
}

exception UnknownStage of string
exception UnknownFormat of string
exception UnknownAccuracy of string

let default_lang = { Language.dialect=Language.WebGL;
                     Language.version=(1,0,0);
                     Language.bond=Language.Warn }

let default_options meta = {
  stage    = Link;
  format   = JSON;
  verbose  = false;
  dissolve = false;
  linectrl = true;
  metadata = meta;
  renames  = [];
  exports  = [];
  symbols  = [];
  base     = Uri.of_string "";
  output   = STDOUT; 
  inputs   = [];
  prologue = [];
  inlang   = default_lang;
  outlang  = default_lang;
  accuracy = Language.Best;
}

let gloc_version = Semver (2, 0, 0)
let gloc_distributor = "Ashima Arts"

let stage_of_string = function
  | "c"      -> Compile
  | "E"      -> Preprocess
  | "e"      -> ParsePP
  | "source" -> Contents
  | "glolli" -> Glolli
  | ""       -> Link
  | stage    -> raise (UnknownStage stage)

let string_of_stage = function
  | Compile    -> "c"
  | Preprocess -> "E"
  | ParsePP    -> "e"
  | Contents   -> "source"
  | Glolli     -> "glolli"
  | Link       -> ""

let format_of_string = function
  | "xml"       -> XML
  | "json" | "" -> JSON
  | format      -> raise (UnknownFormat format)

let string_of_format = function
  | XML  -> "xml"
  | JSON -> "json"

let input_of_string = function
  | "" | "-"  -> STDIN
  | path      -> Stream (Uri.of_string path)

let kvpair_of_input = function
  | STDIN    -> ("",["-"])
  | Stream s -> ("",[Uri.to_string s])
  | Def (m,v)-> ("define", [m^"="^v])

let output_of_string = function
  | "" | "-" -> STDOUT
  | path     -> Path (Uri.of_string path)

let alist_of_output = function
  | STDOUT -> []
  | Path p -> ["o", [Uri.to_string p]]

let accuracy_of_string = function
  | "preprocess" -> Language.Preprocess
  | "best" | ""  -> Language.Best
  | accuracy     -> raise (UnknownAccuracy accuracy)

let get_flag flag params = mem_assoc flag params

let get_flags flags default params =
  try fst (find (fun (x,_) -> mem x flags) params)
  with Not_found -> default

let get_string name default params =
  try assoc name params with Not_found -> default

let get_strings name params = map snd (filter (fun (x,_) -> x = name) params)

(* Maps an assoc list of iface names and strings to an options object *)
let options_of_alist alist = {
  stage    = stage_of_string (get_flags ["c"; "E"; "e"; "source"; "glolli"] "" alist);
  format   = format_of_string (get_flags ["json"; "xml"] "json" alist);
  verbose  = get_flag "v" alist;
  dissolve = get_flag "dissolve" alist;
  linectrl = not (get_flag "line" alist);
  metadata = (match get_string "meta" "" alist with
    | "" -> None | uri -> Some (Uri.of_string uri));
  renames  = concat (get_strings "rename" alist);
  exports  = concat (get_strings "export" alist);
  symbols  = concat (get_strings "u" alist);
  (* TODO: fix order *)
  inputs   = append (map input_of_string (append (get_strings "input" alist)
                                            (get_strings "" alist)))
    (map (fun x -> Def x) (append (get_strings "define" alist)
                             (get_strings "D" alist)));
  output   = output_of_string (get_string "o" "" alist);
  base     = Uri.of_string (get_string "base" "" alist);
  (* TODO: generate prologue from -D *)
  prologue = [];
  inlang   = default_lang;
  outlang  = default_lang;
  accuracy = accuracy_of_string (get_string "accuracy" "best" alist);
}

let make_params name values = map (fun x -> (name, [x])) values

(* TODO: suppress defaults? *)
let alist_of_options options = concat [
  [
    string_of_stage options.stage, [];
    string_of_format options.format, [];
  ];
  if options.verbose  then ["v", []]        else [];
  if options.dissolve then ["dissolve", []] else [];
  if options.linectrl then ["line", []]     else [];
  alist_of_output options.output;
  map kvpair_of_input options.inputs;
  make_params "rename" options.renames;
  make_params "export" options.exports;
  make_params "u"      options.symbols;
  let open Language in [
    "base",     [Uri.to_string options.base];
    "x",        [string_of_dialect options.inlang.dialect];
    "t",        [string_of_dialect options.outlang.dialect];
    "accuracy", [string_of_accuracy options.accuracy];
  ];
]

(* name(s), repeatable, help message *)
let options = [ (* TODO: Add a type specification to each input option *)
  [""], true;                          (* input files... *)
  ["c"; "E"; "e"; "source"; "glolli"], false;
  ["json"; "xml"], false;
  ["u"], true;
  ["define"], true;
  ["D"], true;
  ["o"], false;
  ["accuracy"], false;
  ["line"], false;
  ["x"], false;
  ["t"], false;
  ["dissolve"], false;
  ["rename"], true;
  ["export"], true;
  ["v"], false;
  ["meta"], false;
  ["base"], false;
]

let help = [
  ["", "source input"];
  ["", "produce linked SL"];
  ["c", "produce glo and halt; do not link"];
  ["E", "preprocess and halt; do not parse SL"];
  ["e", "parse preprocessor and halt; do not preprocess"];
  ["source", "strip the glo format and return the contained source"];
  ["glolli", "generate the glolli expression from this command"];
  ["json", "produce JSON (default)"];
  ["xml", "produce XML"];
  ["u", "required symbol (default ['main'])"];
  ["define", "define a macro unit"];
  ["D", "define a global macro"];
  ["o", "output file"];
  ["accuracy", "output accuracy"];
  ["line", "disregard incoming line directives"];
  ["x", "source language"];
  ["t", "target language"];
  ["dissolve", "dissolve declarations"];
  ["rename", "rename identifiers"];
  ["export", "export identifiers"];
  ["v", "verbose"];
  ["meta", "prototypical glo file to use for metadata"];
  ["base", "base URI"];
]
