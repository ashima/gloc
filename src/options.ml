open Semver

type stage = Glolli | Contents | ParsePP | Preprocess | Compile | Link
type format = XML | JSON

type 'a input = Stream of 'a | Def of 'a
    
type 'a options = {
  stage    : stage;
  format   : format;
  verbose  : bool;
  dissolve : bool;
  linectrl : bool;
  metadata : 'a option;
  renames  : string list;
  exports  : string list;
  symbols  : string list;
  base     : string;
  output   : string;                    (* Maybe mark stdout with the type system? *)
  inputs   : string input list;
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
  base     = "";
  output   = "-"; 
  inputs   = [];
  prologue = [];
  inlang   = default_lang;
  outlang  = default_lang;
  accuracy = Language.Best;
}

let gloc_version = Semver (1, 1, 0)
let gloc_distributor = "Ashima Arts"
