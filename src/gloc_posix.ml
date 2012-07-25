module Gloc_posix = Gloc.Make(Platform_posix)
open Gloc_posix
;;

begin try Lwt_main.run (gloc (Cli.options_of_args `Posix Sys.argv))
  with Gloc.Exit code -> exit code
end
