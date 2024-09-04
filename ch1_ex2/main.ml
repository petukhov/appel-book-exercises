type id = string
type binop = Plus | Minus | Times

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

type key_value = id * int

let print_list msg list =
  match list with
  | [] -> print_endline @@ msg ^ " []"
  | _ -> List.iter (fun (k, v) -> Printf.printf "(%s: %d) " k v) list

let rec find (list : key_value list) (key : id) =
  match list with
  | [] -> None
  | (k, v) :: tl -> if k = key then Some v else find tl key

let update (list : key_value list) (key : id) (value : int) =
  match list with [] -> [ (key, value) ] | tl -> (key, value) :: tl

let cmd_opt op (a : int option) (b : int option) =
  match (a, b) with Some a, Some b -> Some (op a b) | _ -> None

let rec interpret_exp exp env =
  match exp with
  | IdExp id -> (find env id, env)
  | NumExp n -> (Some n, env)
  | OpExp (exp1, binop, exp2) -> (
      let v1, env' = interpret_exp exp1 env in
      let v2, env'' = interpret_exp exp2 env' in
      match binop with
      | Plus -> (cmd_opt ( + ) v1 v2, env'')
      | Minus -> (cmd_opt ( - ) v1 v2, env'')
      | Times -> (cmd_opt ( * ) v1 v2, env''))
  | EseqExp (stm, exp) ->
      let env' = interpret_stm stm env in
      interpret_exp exp env'

and interpret_stm stm env =
  match stm with
  | CompoundStm (stm1, stm2) ->
      let env' = interpret_stm stm1 env in
      interpret_stm stm2 env'
  | AssignStm (id, exp) ->
      let v, env' = interpret_exp exp env in
      update env' id (Option.value v ~default:0)
  | PrintStm exp_list ->
      exp_list
      |> List.map (fun exp -> interpret_exp exp env |> fst |> Option.value ~default:0)
      |> List.iter (fun result -> print_endline (string_of_int result));
      env

let () = interpret_stm prog [] |> ignore

(* let test_key_value_list =
     let env = [ ("a", 1); ("b", 2); ("c", 3) ] in
     assert (find env "a" = Some 1);
     assert (find env "b" = Some 2);
     assert (find env "c" = Some 3);
     assert (find env "d" = None);
     assert (update env "a" 10 = [ ("a", 10); ("a", 1); ("b", 2); ("c", 3) ]);
     assert (update env "d" 4 = [ ("d", 4); ("a", 1); ("b", 2); ("c", 3) ])

   let test_key_value_list_advanced =
     let env = [ ("c", 10); ("a", 1); ("b", 2); ("c", 3) ] in
     assert (find env "c" = Some 10);
     assert (find env "a" = Some 1) *)

(* let () =
   let env = [ ("a", 1); ("b", 2); ("c", 3) ] in
   print_list (update env "a" 10) *)
