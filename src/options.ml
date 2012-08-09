open List
open Semver

type stage = Glolli | Contents | ParsePP | Preprocess | Compile | Link
type format = XML | JSON

type input = STDIN | Stream of Uri.t | Def of string (* Stream of uri | Def of string *)
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

let gloc_version = Semver (1, 1, 0)
let gloc_distributor = "Ashima Arts"

(* Use the iface types in my interface specification *)
type group = Stage of stage | Format of format 
type set = LinkSymbols | LinkDefines | LinkGlobalMacros | LinkRenames | LinkExports 
type option_flag = DisableLineControl | Verbose | Dissolve
type filetype = Output | Meta | Input | Base
type iface =
    Group of group
  | Set of set
  | List of iface
  | Filename of filetype
  | Option of option_flag
  | Choice of string list

let stage_of_string = function
  | "c"      -> Compile
  | "E"      -> Preprocess
  | "e"      -> ParsePP
  | "source" -> Contents
  | "glolli" -> Glolli
  | _        -> Link

let format_of_string = function
  | "xml" -> XML
  | _     -> JSON

let input_of_string = function
  | "-"  -> STDIN
  | path -> Stream (Uri.of_string path)

let output_of_string = function
  | ""   -> STDOUT
  | path -> Path (Uri.of_string path)

let accuracy_of_string = function
  | "preprocess" -> Language.Preprocess
  | _            -> Language.Best

let get_flag flag params = exists (fun x -> fst x = flag) params

let get_flags flags default params = try fst (find (fun x -> mem (fst x) flags) params)
  with Not_found -> default

let get_string name default params = try snd (find (fun x -> fst x = name) params)
  with Not_found -> default

let get_strings name params = map snd (filter (fun x -> fst x = name) params)

(* Maps an assoc list of iface names and strings to an options object *)
let options_of_iface iface = {
  stage = stage_of_string (get_flags ["c"; "E"; "e"; "source"; "glolli"] "" iface);
  format = format_of_string (get_flags ["json"; "xml"] "json" iface);
  verbose = get_flag "v" iface;
  dissolve = get_flag "dissolve" iface;
  linectrl = get_flag "line" iface;
  metadata = None;
  renames = get_strings "rename" iface;
  exports = get_strings "export" iface;
  symbols = get_strings "u" iface;
  inputs = append (map input_of_string (append (get_strings "input" iface)
                                          (get_strings "" iface)))
    (map (fun x -> Def x) (append (get_strings "define" iface)
                          (get_strings "D" iface))); (* This bit makes it look like Lisp... *)
  output = output_of_string (get_string "o" "" iface);
  base = Uri.of_string (get_string "base" "" iface);
  prologue = [];
  inlang = default_lang;
  outlang = default_lang;
  accuracy = accuracy_of_string (get_string "accuracy" "best" iface);
}

(* name(s), repeatable, type*)
let options = [
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
