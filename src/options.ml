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
