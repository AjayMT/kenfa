
open Base
open Kenfa

let () =
  let usage_msg =
    "kenfa <regexp> <input>\nkenfa -d <regexp> [<input>]\n\nOptions:"
  in
  let args = ref [] in
  let draw_flag = ref false in
  let options = [
      ("-d", Caml.Arg.Set draw_flag, "\tOutput the generated DFA as a DOT diagram.")
    ] in
  let () = Caml.Arg.parse options (fun a -> args := a :: !args) usage_msg in
  match (List.rev !args) with
  | [] -> Caml.Arg.usage options usage_msg
  | regexp :: rest ->
     match compile_regexp regexp with
     | Error e -> Stdlib.prerr_endline e
     | Ok dfa ->
        if !draw_flag then Stdlib.print_endline @@ draw_dfa dfa else ();
        match (rest, !draw_flag) with
        | (input :: _, _) -> Stdlib.print_endline @@ Bool.to_string @@
                               string_match dfa input
        | ([], false) -> Caml.Arg.usage options usage_msg
        | _ -> ()
