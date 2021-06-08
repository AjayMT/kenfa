
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

let parse_regexp re =
  let rec aux re nodes expect_paren escaped = match (re, escaped) with
    | ([], _) -> Ok (nodes, [])
    | (')' :: re, false) ->
       if expect_paren then Ok (nodes, re) else Error "unexpected ')'"
    | ('(' :: re, false) ->
       let* (child_nodes, remaining) = aux re [] true false in
       aux remaining ((Group (List.rev child_nodes)) :: nodes) expect_paren false
    | (a :: re, _) ->
       match (a, escaped) with
       | ('+', false) | ('*', false) | ('?', false) ->
          begin match nodes with
          | n :: nodes ->
             let node = match a with
               | '+' -> Plus n
               | '*' -> Star n
               | _ -> Once n
             in
             aux re (node :: nodes) expect_paren false
          | [] -> Error ("unexpected " ^ (String.of_char a))
          end
       | ('|', false) ->
          begin match nodes with
          | _ :: _ ->
             let* (next_nodes, remaining) = aux re [] expect_paren false in
             begin match next_nodes with
             | _ :: _ ->
                aux remaining
                  [Alternation (Group (List.rev nodes), Group (List.rev next_nodes))]
                  expect_paren false
             | [] -> Error "unexpected '|'"
             end
          | [] -> Error "unexpected '|'"
          end
       | ('\\', false) -> aux re nodes expect_paren true
       | (a, _) -> aux re ((Atom a) :: nodes) expect_paren false
  in
  let+ (nodes, _) = aux re [] false false in
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

let dfa_of_nfa nfa =
  let nfa_arr =
    Array.of_list @@
      List.map ~f:(fun (_, a) -> a) @@
        List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) nfa
  in

  let rec traverse_nfa state =
    let (start_dfa_node, edge_char, dfa_nodes, dfa_edges) = state in
    let rec traverse_nfa_node_until_match visited n =
      if Set.mem visited n then []
      else
        let visited = Set.add visited n in
        match (Array.get nfa_arr n) with
        | Continue id -> traverse_nfa_node_until_match visited id
        | Split (a, b) -> (traverse_nfa_node_until_match visited a)
                          @ (traverse_nfa_node_until_match visited b)
        | Accept -> [n]
        | Match (_, _) -> [n]
    in

    let step_forward c n = match (Array.get nfa_arr n) with
      | Continue id -> [id]
      | Split (a, b) -> [a; b]
      | Accept -> []
      | Match (d, id) -> if Char.equal d c then [id] else []
    in
    let next_dfa_node =
      List.sort ~compare:Int.compare @@
        List.bind ~f:(traverse_nfa_node_until_match (Set.empty (module Int))) @@
          match edge_char with
          | Some c -> List.bind start_dfa_node ~f:(step_forward c)
          | None -> start_dfa_node
    in
    (* TODO: don't add duplicate edges *)
    let new_dfa_edges = match edge_char with
      | Some c -> (c, start_dfa_node, next_dfa_node) :: dfa_edges
      | None -> dfa_edges
    in
    if List.mem dfa_nodes next_dfa_node ~equal:(List.equal (=)) then
      (start_dfa_node, edge_char, dfa_nodes, new_dfa_edges)
    else begin
        let explore_match state n = match (Array.get nfa_arr n) with
          | Match (c, _) ->
             let (start_dfa_node, _, dfa_nodes, dfa_edges) = state in
             let (_, _, dfa_nodes, dfa_edges) =
               traverse_nfa (start_dfa_node, Some c, dfa_nodes, dfa_edges)
             in
             (start_dfa_node, None, dfa_nodes, dfa_edges)
          | _ -> state
        in
        let new_state =
          (next_dfa_node, None, next_dfa_node :: dfa_nodes, new_dfa_edges)
        in
        List.fold next_dfa_node ~init:new_state ~f:explore_match
      end
  in
  let (_, _, dfa_nodes, dfa_edges) = traverse_nfa ([0], None, [], []) in
  (dfa_nodes, dfa_edges)

let simplify_dfa (dfa_nodes, dfa_edges) accept_nfa_node =
  (* we assume the last element of dfa_nodes is the starting node *)
  let nodes_with_id =
    List.zip_exn (List.range 0 (List.length dfa_nodes)) (List.rev dfa_nodes)
  in
  let id_edge (c, a, b) =
    let (a_id, _) =
      List.find_exn nodes_with_id ~f:(fun (_, ids) -> List.equal (=) a ids)
    in
    let (b_id, _) =
      List.find_exn nodes_with_id ~f:(fun (_, ids) -> List.equal (=) b ids)
    in
    (c, a_id, b_id)
  in
  let edges_with_id = List.map dfa_edges ~f:id_edge in
  let find_edges (id, _) =
    let add_edge edges (c, a, b) =
      if a = id then
        match Map.add edges ~key:c ~data:b with
        | `Ok m -> m
        | _ -> edges
      else edges
    in
    let edges =
      List.fold ~init:(Map.empty (module Char)) ~f:add_edge edges_with_id
    in
    (id, edges)
  in
  let edge_maps = List.map nodes_with_id ~f:find_edges in
  let fsm =
    Array.of_list @@ List.map ~f:(fun (_, a) -> a) @@
      List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) edge_maps
  in
  let add_accepted accepted (id, nfa_nodes) =
    if List.mem nfa_nodes accept_nfa_node ~equal:(=) then id :: accepted
    else accepted
  in
  let accepted = List.fold_left nodes_with_id ~init:[] ~f:add_accepted in
  (fsm, accepted)

let compile_regexp re =
  let+ regexp_nodes = parse_regexp (String.to_list re) in
  let nfa_nodes = nfa_of_regexp_nodes regexp_nodes in
  let accept_nfa_node = (List.length nfa_nodes) - 1 in
  simplify_dfa (dfa_of_nfa nfa_nodes) accept_nfa_node

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

let draw_dfa (fsm, accepted) =
  let add_edges id edges m =
    let add_edge_map ~key:k ~data:v edges =
      let s =
        (Int.to_string id) ^ " -> " ^ (Int.to_string v) ^ " [ "
        ^ "label=\"" ^ (Char.to_string k) ^ "\" ];"
      in
      s :: edges
    in
    Map.fold m ~init:edges ~f:add_edge_map
  in
  let edges = Array.foldi fsm ~init:[] ~f:add_edges in
  let add_accept id accepted_nodes _ =
    if List.mem accepted id ~equal:(=) then
      ((Int.to_string id) ^ " [ peripheries=2 ];") :: accepted_nodes
    else accepted_nodes
  in
  let accepted_nodes = Array.foldi fsm ~init:[] ~f:add_accept in
  "digraph DFA {\n" ^ (String.concat ~sep:"\n" edges) ^ "\n" ^
    (String.concat ~sep:"\n" accepted_nodes) ^ "\n}"
