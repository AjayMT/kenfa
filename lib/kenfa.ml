
open Base

(* dfa is an array of maps of chars to ints and a list of "accepted" states.
   each state is an index in the array (i.e an int)
   and the arrows from that state are the entries in the map.
   0 is the start state. *)
type dfa = ((char, int, Char.comparator_witness) Map.t) array * int list

let compile_regexp _re = Error "unimplemented"

let string_match fsm s =
  let (graph, accepted) = fsm in
  let transition curr_state char = Map.find_exn (Array.get graph curr_state) char in
  let final_state = String.fold s ~init:0 ~f:transition in
  List.mem accepted final_state ~equal:(phys_equal)
