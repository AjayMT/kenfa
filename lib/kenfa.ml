
open Base

(* dfa is an array of maps of chars to ints and a list of "accepted" states.
   each state is an index in the array (i.e an int)
   and the arrows from that state are the entries in the map.
   0 is the start state. *)
type dfa = ((char, int, Char.comparator_witness) Map.t) array * int list

type regexp_node = Atom of char
                 | Plus of regexp_node
                 | Star of regexp_node
                 | Once of regexp_node
                 | Alternation of regexp_node * regexp_node
                 | Group of regexp_node list

let ( let* ) x f = Result.bind x ~f:f
let ( let+ ) x f = Result.map ~f:f x

let rec show_node node = match node with
  | Atom a -> String.of_char a
  | Plus n | Star n | Once n ->
     let substr = show_node n in
     let op = match node with
       | Plus _ -> "+ " | Star _ -> "* " | _ -> "? "
     in
     "(" ^ op ^ substr ^ ")"
  | Alternation (one, two) ->
     let (a, b) = (show_node one, show_node two) in
     "(" ^ a ^ " | " ^ b ^ ")"
  | Group nodes ->
     "(" ^ (String.concat ~sep:" " @@ List.map ~f:show_node nodes) ^ ")"

let parse_regexp re =
  let rec aux re nodes expect_paren = match re with
    | [] -> Ok (nodes, [])
    | ')' :: re -> if expect_paren then Ok (nodes, re) else Error "unexpected ')'"
    | '(' :: re ->
       let* (child_nodes, remaining) = aux re [] true in
       aux remaining ((Group (List.rev child_nodes)) :: nodes) expect_paren
    | a :: re ->
       match a with
       | '+' | '*' | '?' ->
          begin match nodes with
          | n :: nodes ->
             let node = match a with
               | '+' -> Plus n
               | '*' -> Star n
               | _ -> Once n
             in
             aux re (node :: nodes) expect_paren
          | [] -> Error ("unexpected " ^ (String.of_char a))
          end
       | '|' ->
          begin match nodes with
          | n :: nodes ->
             let* (next_nodes, remaining) = aux re nodes expect_paren in
             begin match next_nodes with
             | n2 :: nodes ->
                aux remaining ((Alternation (n, n2)) :: nodes) expect_paren
             | [] -> Error "unexpected '|'"
             end
          | [] -> Error "unexpected '|'"
          end
       | a -> aux re ((Atom a) :: nodes) expect_paren
  in
  let+ (nodes, _) = aux re [] false in
  List.rev nodes

let compile_regexp re =
  match parse_regexp (String.to_list re) with
  | Ok nodes -> Error (String.concat ~sep:" " @@ List.map ~f:show_node nodes)
  | Error e -> Error e

let string_match fsm s =
  let (graph, accepted) = fsm in
  let transition curr_state char =
    let m = Array.get graph curr_state in
    match Map.find m char with
    | Some next_state -> Ok next_state
    | None -> Error ()
  in
  let final_state = String.fold_result s ~init:0 ~f:transition in
  match final_state with
  | Ok final_state -> List.mem accepted final_state ~equal:phys_equal
  | Error () -> false
