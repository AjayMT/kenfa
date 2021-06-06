
open Base
open Kenfa

let () =
  let regexp_string = Array.get (Sys.get_argv ()) 1 in
  match compile_regexp regexp_string with
  | Error e -> Stdlib.prerr_endline e
  | Ok dfa -> Stdlib.print_endline @@ draw_dfa dfa
