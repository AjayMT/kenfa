
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

(* let rec show_node node = match node with
 *   | Atom a -> String.of_char a
 *   | Plus n | Star n | Once n ->
 *      let substr = show_node n in
 *      let op = match node with
 *        | Plus _ -> "+ " | Star _ -> "* " | _ -> "? "
 *      in
 *      "(" ^ op ^ substr ^ ")"
 *   | Alternation (one, two) ->
 *      let (a, b) = (show_node one, show_node two) in
 *      "(" ^ a ^ " | " ^ b ^ ")"
 *   | Group nodes ->
 *      "(" ^ (String.concat ~sep:" " @@ List.map ~f:show_node nodes) ^ ")" *)

let ( let* ) x f = Result.bind x ~f:f
let ( let+ ) x f = Result.map ~f:f x

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
          | _ :: _ ->
             let* (next_nodes, remaining) = aux re [] expect_paren in
             begin match next_nodes with
             | _ :: _ ->
                aux remaining
                  [Alternation (Group (List.rev nodes), Group (List.rev next_nodes))]
                  expect_paren
             | [] -> Error "unexpected '|'"
             end
          | [] -> Error "unexpected '|'"
          end
       | a -> aux re ((Atom a) :: nodes) expect_paren
  in
  let+ (nodes, _) = aux re [] false in
  List.rev nodes

type nfa_node = Accept
              | Continue of int
              | Split of int * int
              | Match of char * int

let nfa_of_regexp_nodes nodes =
  let rec aux state node =
    let (next_id, nfa_nodes) = state in
    match node with
    | Atom a ->
       let nfa_node = Match (a, next_id + 1) in
       (next_id + 1, (next_id, nfa_node) :: nfa_nodes)
    | Group nodes -> List.fold_left nodes ~init:(next_id, nfa_nodes) ~f:aux
    | Alternation (a, b) ->
       let split_node_id = next_id in
       let first_branch_id = next_id + 1 in
       let (first_branch_target, nfa_nodes) = aux (first_branch_id, nfa_nodes) a in
       let second_branch_id = first_branch_target + 1 in
       let (second_branch_target, nfa_nodes) =
         aux (second_branch_id, nfa_nodes) b
       in
       let continue_node_pair = (first_branch_target, Continue second_branch_target) in
       let split_node_pair =
         (split_node_id, Split (first_branch_id, second_branch_id))
       in
       (second_branch_target,
        split_node_pair :: continue_node_pair :: nfa_nodes)
    | Once n ->
       let split_node_id = next_id in
       let match_branch_id = next_id + 1 in
       let (match_target, nfa_nodes) = aux (match_branch_id, nfa_nodes) n in
       let split_node_pair = (split_node_id, Split (match_target, match_branch_id)) in
       (match_target, split_node_pair :: nfa_nodes)
    | Star n ->
       let split_node_id = next_id in
       let match_branch_id = next_id + 1 in
       let (match_target, nfa_nodes) = aux (match_branch_id, nfa_nodes) n in
       let second_split_node_pair =
         (match_target, Split (match_branch_id, match_target + 1))
       in
       let split_node_pair =
         (split_node_id, Split (match_branch_id, match_target + 1))
       in
       (match_target + 1, split_node_pair :: second_split_node_pair :: nfa_nodes)
    | Plus n ->
       let (next_id, nfa_nodes) = aux (next_id, nfa_nodes) n in
       aux (next_id, nfa_nodes) (Star n)
  in
  let (final_target, nfa_nodes) = List.fold_left nodes ~init:(0, []) ~f:aux in
  (final_target, Accept) :: nfa_nodes

let _dfa_of_nfa nfa =
  let nfa_arr =
    Array.of_list @@
      List.map ~f:(fun (_, a) -> a) @@
        List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) nfa
  in

  let traverse_nfa state =
    let (start_dfa_node, edge_char, dfa_nodes, dfa_edges) = state in
    let rec traverse_nfa_node_until_match n = match (Array.get nfa_arr n) with
      | Continue id -> traverse_nfa_node_until_match id
      | Split (a, b) -> (traverse_nfa_node_until_match a)
                        @ (traverse_nfa_node_until_match b)
      | Accept -> [n]
      | Match (_, _) -> [n]
    in

    (*
      TODO: advance forward by one step using edge_char before applying
      traverse_nfa_node_until_match
     *)
    let next_dfa_node =
      List.bind start_dfa_node ~f:traverse_nfa_node_until_match
    in
    (next_dfa_node, edge_char, dfa_nodes, dfa_edges)
  in
  let (_, _, dfa_nodes, dfa_edges) = traverse_nfa ([0], None, [[0]], []) in
  (dfa_nodes, dfa_edges)

let compile_regexp re =
  let show_nfa_node (id, variant) =
    let vs = match variant with
      | Accept -> "A" | Continue i -> "C " ^ (Int.to_string i)
      | Split (a, b) -> "S " ^ (Int.to_string a) ^ " " ^ (Int.to_string b)
      | Match (c, i) -> "M " ^ (Char.to_string c) ^ " " ^ (Int.to_string i)
    in
    "(" ^ (Int.to_string id) ^ ", " ^ vs ^ ")"
  in
  let+ regexp_nodes = parse_regexp (String.to_list re) in
  let nfa_nodes = nfa_of_regexp_nodes regexp_nodes in
  String.concat ~sep:" " @@ List.map nfa_nodes ~f:show_nfa_node

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
  | Ok final_state -> List.mem accepted final_state ~equal:(=)
  | Error () -> false
