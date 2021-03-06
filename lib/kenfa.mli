
(** The type of compiled regular expressions. *)
type dfa

(** [compile_regexp s] compiles the regular expression string [s] into a DFA. *)
val compile_regexp : string -> (dfa, string) result

(** [string_match d s] checks whether the string [s] matches the DFA [d] exactly. *)
val string_match : dfa -> string -> bool

(** [draw_dfa d] outputs a DOT diagram that represents the DFA [d]. *)
val draw_dfa : dfa -> string
