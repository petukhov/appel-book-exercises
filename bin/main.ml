open Base
(* open Stdio *)

type id = string

type binop = Plus | Minus | Times

type stm = CompoundStm of stm * stm
     | AssignStm of id * exp
     | PrintStm of exp list

and exp = IdExp of id
     | NumExp of int
     | OpExp of exp * binop * exp
     | EseqExp of stm * exp

let table = Hashtbl.create (module String)

let prog =
  CompoundStm (
    AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
    CompoundStm (
      AssignStm (
        "b",
        EseqExp (
          PrintStm [IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1)],
          OpExp (NumExp 10, Times, IdExp "a")
        )
      ),
      PrintStm [IdExp "b"]
    )
  )

let rec interpret_exp exp =
    match exp with
    | IdExp(id) -> 
        Hashtbl.find_exn table id
    | NumExp(n) -> 
        n
    | OpExp(exp1, binop, exp2) -> 
        let v1 = interpret_exp exp1 in
        let v2 = interpret_exp exp2 in
        (match binop with
        | Plus -> v1 + v2
        | Minus -> v1 - v2
        | Times -> v1 * v2)
    | EseqExp(stm, exp) -> 
        interpret_stm stm;
        interpret_exp exp

and interpret_stm stm = 
  match stm with
  | CompoundStm(stm1, stm2) -> 
      interpret_stm stm1;
      interpret_stm stm2;
  | AssignStm(id, exp) -> 
      Hashtbl.set table ~key:id ~data:(interpret_exp exp)
  | PrintStm(exp_list) -> 
      List.iter exp_list ~f:(fun exp -> Stdio.print_endline (Int.to_string (interpret_exp exp)))
      
let () =
  interpret_stm prog



  
