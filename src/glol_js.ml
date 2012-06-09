open Js
open Json
open Dom_html
open Glo_lib

external reg : string -> ('a -> 'b) -> unit = "register_ocaml_fn"

module Linker = Glol.Make(World.Make(struct
  include Platform_js
  let glo_of_source = None
end))

let string_of_js_exc e = string (Glol.string_of_error begin
  match to_string (e##name) with
    | "MissingMacro" ->
      Glol.MissingMacro (to_string e##addr, to_string e##macro)
    | "MissingSymbol" ->
      Glol.MissingSymbol (to_string e##addr, to_string e##symbol)
    | "NotFound" ->
      Failure ("NotFound: "^(to_string e##key))
    | "CircularDependency" ->
      let addrs = Array.to_list (to_array e##addrs) in
      Glol.CircularDependency (List.map to_string addrs)
    | "SymbolConflict" ->
      Glol.SymbolConflict (to_string (e##sym_a_),
                           to_string (e##sym_b_),
                           to_string (e##addr_a_),
                           to_string (e##addr_b_))
    | "UnknownBehavior" ->
      Glol.UnknownBehavior (to_string e##addr, to_string e##behavior)
    | "UnknownGloVersion" ->
      let v = to_array e##version in
      Glol.UnknownGloVersion (to_string e##path, (v.(0),v.(1),v.(2)))
    | n -> Failure ("unknown error "^n)
end)
in

let link prologue syms glom_s callback errback =
  let gloms = to_string glom_s in
  let glom = glom_of_string gloms in
  try_lwt lwt seq = Linker.satisfy
      (List.map to_string (Array.to_list (to_array syms)))
      (Linker.stream_of_glom ~src:"[glol.js]" glom)
    in Lwt.return
    (Js.Unsafe.fun_call callback
       [|Js.Unsafe.inject (string (Linker.link (to_string prologue) seq))|])
  with e -> Lwt.return
    (Js.Unsafe.fun_call errback
       [|Js.Unsafe.inject (string (Glol.string_of_error e))|])
in
reg "link" link;
reg "string_of_error" (fun e -> string (Glol.string_of_error e));
reg "string_of_js_exc" string_of_js_exc
