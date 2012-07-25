open Js

external reg : string -> ('a -> 'b) -> unit = "register_ocaml_fn"

module Gloc_js = Gloc.Make(struct
  include Platform_js
  let glo_of_source =
    let open Gloc_lib in
        Some (fun path src ->
          compile {(new_exec_state None) with
            stage = ref Compile;
            base = ref path
          } path src)
end)
open Gloc_js

let gloc args callback errback =
  let args = Array.map to_string (to_array args) in
  let args = Array.of_list ("gloc"::(Array.to_list args)) in
  try
    options_of_args args
    begin try_lwt
      lwt () = gloc exec_state in
      Lwt.return (Js.Unsafe.fun_call callback [||])
    with
      | Gloc.Exit c ->
        (*lwt () = Platform_js.eprint ("Exit "^(string_of_int c)^"\n") in*)
        Lwt.return (Js.Unsafe.fun_call errback [||])
      | Gloc_lib.CompilerError(_,el) -> (* TODO: FIXME *)
        lwt () = Platform_js.eprint "CompilerError:\n" in
        lwt () = Lwt_list.iter_s
          (fun exn -> Platform_js.eprint ((Printexc.to_string exn)^"\n"))
          el in
          Lwt.return (Js.Unsafe.fun_call errback [||])
      | e -> lwt () = Platform_js.eprint ((Printexc.to_string e)^"\n") in
           Lwt.return (Js.Unsafe.fun_call errback [||])
    end
  with Arg.Help s ->
    lwt () = Platform_js.eprint (s^"\n") in
      Js.Unsafe.fun_call callback [||]
;;
reg "gloc" gloc
