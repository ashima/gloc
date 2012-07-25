(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

module Lang = Language
open Printf
open Pp_lib
open Pp
open Esslpp_lex
open Glo_lib
open Options

module List = struct
  include List
  let unique l =
    let h = Hashtbl.create (List.length l) in
    let l = List.fold_left
      (fun l v -> if Hashtbl.mem h v
        then l else begin Hashtbl.replace h v (); v::l end)
      [] l
    in List.rev l
end

(* TODO: harmonize? *)
type component =
  | Command
  | Format
  | Parser
  | PPParser
  | PPError
  | PPDiverge
  | SLParser
  | Analyzer
  | Linker
  | WriteOut

let exit_code_of_component = function
  | Command -> 1
  | Format -> 2
  | Parser -> 3
  | PPParser -> 4
  | PPError -> 5
  | PPDiverge -> 6
  | SLParser -> 7
  | Analyzer -> 8
  | Linker -> 9
  | WriteOut -> 10

let string_of_component_error = function
  | Command -> "unrecoverable command error"
  | Format -> "unrecoverable format read error"
  | Parser -> "unrecoverable internal parser error"
  | PPParser -> "unrecoverable preprocessor parse error"
  | PPError -> "unrecoverable preprocessor error"
  | PPDiverge -> "unrecoverable preprocessor divergence"
  | SLParser -> "unrecoverable parse error"
  | Analyzer -> "unrecoverable analysis error"
  | Linker -> "unrecoverable link error"
  | WriteOut -> "unrecoverable output error"

exception UnrecognizedJsonFormat
exception UncaughtException of exn
exception AmbiguousPreprocessorConditional of string pptok
exception MultiUnitGloPPOnly of string (* TODO: error message, test *)
exception GlomPPOnly of string
exception IncompatibleArguments of string * string
exception GloppStageError of stage
exception InvalidDefine (* TODO: error message *)
exception InvalidRenameString of string (* TODO: error message, test *)
exception MetaPrototypeFailure of string
exception OutputFailure of string * string

exception CompilerError of component * exn list

let maybe_fatal_error k =
  if (List.length !errors) > 0
  then begin let errs = !errors in
             errors := [];
             raise (CompilerError (k,errs))
  end

let check_pp_divergence ppl =
  if List.length ppl > 1 then
    let o = List.fold_left
      (fun dl pp -> List.fold_left
        (fun dl om ->
          if List.exists (fun m -> om.v=m.v) dl then dl else om::dl)
        dl (fst pp).inmacros)
      [] ppl
    in raise (CompilerError
                (PPDiverge,
                 List.rev_map (fun t -> AmbiguousPreprocessorConditional t) o))
  else match ppl with (_,e)::_ -> e
    | [] -> Chunk { span={a=start_loc;z=start_loc};
                    scan=(fun loc -> (loc,""));
                    comments=([],ref []);
                    v=[] }

let parse lang source =
  let p = try parse lang source with
    | err -> raise (CompilerError (Parser, [UncaughtException err]))
  in maybe_fatal_error PPParser; p

let preprocess lang ppexpr =
  let ppl = preprocess_ppexpr (Glo.empty_ppenv lang) ppexpr in
  maybe_fatal_error PPError;
  ppl

let compile options path source =
  let ppexpr = parse options.inlang source in
  let ppl = preprocess options.inlang ppexpr in

  let ppexpr = if (options.accuracy=Lang.Preprocess
                  && options.stage=Compile)
    then check_pp_divergence ppl
    else ppexpr
  in

  let meta = options.metadata in
  try (if options.dissolve
    then Glo.dissolve else Glo.compile)
        ?meta options.inlang path ppexpr ppl
  with (CompilerError _) as e -> raise e
    | err -> raise (CompilerError (SLParser, [err]))

let split_define_string ds =
  match Re_str.bounded_split (Re_str.regexp_string "=") ds 2 with
    | [] -> raise InvalidDefine
    | bare::[] -> (bare, None)
    | m::d::_ -> (m, Some d)

let make_define_line ds =
  match split_define_string ds with
    | bare, None -> "#define "^bare^"\n"
    | m, Some d -> "#define "^m^" "^d^"\n"

let glo_of_u meta target u =
  {glo=glo_version; target; meta; units=[|u|]; linkmap=[]}

let make_glo options path s =
  match glom_of_string s with
    | Source s -> compile options path s
    | glom -> glom

let minimize_glom = function
  | Glom [_,Leaf glo] -> Leaf glo
  | x -> x

let string_of_error linectrl = function
  | UnknownBehavior t ->
    sprintf "%s:\nunknown behavior \"%s\"\n" (string_of_span linectrl t.span) t.v
  | UnterminatedComment t ->
    sprintf "%s:\nunterminated comment\n" (string_of_span linectrl t.span)
  | UnterminatedConditional t ->
    sprintf "%s:\nunterminated conditional \"%s\"\n" (string_of_span linectrl t.span)
      (snd (t.scan t.span.a))
  | UnknownCharacter t ->
    sprintf "%s:\nunknown character '%s'\n" (string_of_span linectrl t.span)
      (snd (t.scan t.span.a))
  | LineContinuationUnsupported t ->
    sprintf "%s:\nline continuation officially unsupported\n"
      (string_of_span linectrl t.span)
  | InvalidDirectiveLocation t ->
    sprintf "%s:\ninvalid directive location\n" (string_of_span linectrl t.span)
  | InvalidDirective t ->
    sprintf "%s:\ninvalid directive \"%s\"\n" (string_of_span linectrl t.span) t.v
  | InvalidOctal t ->
    sprintf "%s:\ninvalid octal constant \"%s\"\n" (string_of_span linectrl t.span) t.v
  | HolyVersion t ->
    sprintf "%s:\nversion must be first semantic token\n"
      (string_of_span linectrl t.span)
  | UnsupportedVersion t ->
    sprintf "%s:\nversion %d is unsupported\n" (string_of_span linectrl t.span) t.v
  | InvalidVersionBase t ->
    sprintf "%s:\nversion must be specified in decimal\n"
      (string_of_span linectrl t.span)
  | InvalidLineBase t ->
    sprintf "%s:\nline control arguments must be specified in decimal\n"
      (string_of_span linectrl t.span)
  | InvalidVersionArg t ->
    sprintf "%s:\ninvalid version argument\n" (string_of_span linectrl t.span)
  | InvalidLineArg t ->
    sprintf "%s:\ninvalid line argument\n" (string_of_span linectrl t.span)
  | MacroArgUnclosed t ->
    sprintf "%s:\nunclosed macro argument list\n" (string_of_span linectrl t.span)
  | MacroArgInnerParenUnclosed t ->
    sprintf "%s:\nunclosed inner parenthesis in macro argument list\n"
      (string_of_span linectrl t.span)
  | MacroArgTooFew (t,a,e) ->
    sprintf "%s:\ntoo few macro arguments: expected %d, got %d\n"
      (string_of_span linectrl t.span) e a
  | MacroArgTooMany (t,a,e) ->
    sprintf "%s:\ntoo many macro arguments: expected %d, got %d\n"
      (string_of_span linectrl t.span) e a
  | ReservedKeyword t ->
    sprintf "%s:\n\"%s\" is a reserved keyword and may not be used\n"
      (string_of_span linectrl t.span) t.v
  | RedefineReservedMacro t ->
    sprintf "%s:\n\"%s\" is a reserved macro and may not be redefined\n"
      (string_of_span linectrl t.span) t.v
  | UndefineReservedMacro t ->
    sprintf "%s:\n\"%s\" is a reserved macro and may not be undefined\n"
      (string_of_span linectrl t.span) t.v
  | ErrorDirective t ->
    sprintf "%s:\n%s\n" (string_of_span linectrl t.span) (snd (t.scan t.span.a))
  | UnsupportedPPOp t ->
    sprintf "%s:\n\"%s\" is not supported in preprocessor expressions\n"
      (string_of_span linectrl t.span) (snd (t.scan t.span.a))
  | FloatUnsupported t ->
    sprintf "%s:\nfloating point is not supported in preprocessor expressions\n"
      (string_of_span linectrl t.span)
  | PPCondExprParseError t ->
    sprintf "%s:\nerror parsing conditional expression \"%s\"\n"
      (string_of_span linectrl t.span) (snd (t.scan t.span.a))
  | UncaughtException e -> sprintf "Uncaught exception:\n%s\n" (Printexc.to_string e)
  | AmbiguousPreprocessorConditional t ->
    sprintf "%s:\nambiguous preprocessor conditional branch: %s\n"
      (string_of_span linectrl t.span) t.v
  | GlomPPOnly path ->
    sprintf "Source '%s' is a glom with multiple source units.\n" path
  | MultiUnitGloPPOnly path -> (* TODO: test *)
    sprintf "Source '%s' is a glo with multiple source units.\n" path
  | Essl_lib.EsslParseError (name,span) ->
    sprintf "%s:\nESSL parse error: %s\n" (string_of_span linectrl span) name
  | Sys_error m -> sprintf "System error:\n%s\n" m
  | exn -> try Glol.string_of_error exn with e -> raise e
