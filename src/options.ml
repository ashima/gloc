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

type argument_type = Flag
                   | Filepath           (* A file path or std(in|out) *)
                   | Choice of string list
                   | String             (* Code and symbols *)

(* name(s), repeatable, type*)
let options = [
  [""], true, Filepath;                          (* input files... *)
  ["c"; "E"; "e"; "source"; "glolli"], false, Flag;
  ["json"; "xml"], false, Flag;
  ["u"], true, String;
  ["define"], true, String;
  ["D"], true, String;
  ["o"], false, Filepath;
  ["accuracy"], false, Choice ["best"; "preprocess"];
  ["line"], false, Flag;
  ["x"], false, Choice ["webgl"];
  ["t"], false, Choice ["webgl"];
  ["dissolve"], false, Flag;
  ["rename"], true, String;
  ["export"], true, String;
  ["v"], false, Flag;
  ["meta"], false, Filepath;
  ["base"], false, Filepath;
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
