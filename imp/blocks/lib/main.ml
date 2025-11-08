open Types
open Ast

(******************************** FORMAT OUTPUT ********************************)
let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Var c -> c
  | Const n -> string_of_int n
  | Not e -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Add(e1,e2) -> "Add(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Sub(e1,e2) -> "Sub(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Mul(e1,e2) -> "Mul(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Eq(e1,e2) -> "Eq(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Leq(e1,e2) -> "Leq(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"

type errors = TYPE | UNBOUND_IDE

let error t e m = match t with
    TYPE -> raise(TypeError ((string_of_expr e) ^ m))
  | UNBOUND_IDE -> raise(UnboundVar ((string_of_expr e) ^ m))

(*************** INPUT FORMAT **************)
let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let apply st x = match topenv st x with
    IVar l -> getmem st l
  | BVar l -> getmem st l


(*************************** BIG STEP ***********************************)
let rec eval_expr (st : state) = function
    True -> Bool true
  | False -> Bool false
  | Var v -> apply st v
  | Const n -> Int n
  | Not e -> (match (eval_expr st e) with
        Bool b -> Bool (not b) | _ -> error TYPE e " invalid_int_argument")
  | And(e1,e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> error TYPE (And(e1,e2)) " invalid_int_argument")
  | Or(e1,e2)  -> (match (eval_expr st e1, eval_expr st e2) with
        (Bool b1, Bool b2) -> Bool (b1 || b2)
      | _ -> error TYPE (Or(e1,e2)) " invalid_int_argument")
  | Add(e1,e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Int n1, Int n2) -> Int (n1+n2)
      | _ -> error TYPE (Add(e1,e2)) " invalid_bool_argument")
  | Sub(e1,e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Int n1, Int n2) -> Int (n1-n2)
      | _ -> error TYPE (Sub(e1,e2)) " invalid_bool_argument")
  | Mul(e1,e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Int n1, Int n2) -> Int (n1*n2)
      | _ -> error TYPE (Mul(e1,e2)) " invalid_bool_argument")
  | Eq(e1,e2)  -> (match (eval_expr st e1, eval_expr st e2) with
        (Int n1, Int n2) -> Bool (n1=n2)
      | _ -> error TYPE (Eq(e1,e2)) " invalid_bool_argument")
  | Leq(e1,e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Int n1, Int n2) -> Bool (n1<=n2)
      | _ -> error TYPE (Leq(e1,e2)) " invalid_bool_argument")


let rec eval_decl st decl : state = match decl with
    [] -> failwith "<decl list> is empty"
  | IntVar n :: _ -> let loc = getloc st in
                     let env = bind_env (topenv st) n (IVar loc) in
                     

    
(************************ SMALL STEP *************************)
let rec trace1 = function
    St _ -> raise(NoRuleApplies)
  | Cmd (Skip,st) -> St st
  | Cmd (Assign(x,v), st) -> 
    
  | Cmd (Seq(c1,c2), st) -> (match trace1 (Cmd (c1,st)) with
        St st' -> Cmd(c2,st')
      | Cmd(c1',st') -> Cmd(Seq(c1',c2), st') )
  | Cmd (If(b,c1,c2),st) -> if (eval_expr st b = Bool true)
                            then (trace1 (Cmd(c1,st)))
                            else (trace1 (Cmd(c2,st)))
  | Cmd (While(b,c),st) -> (match (eval_expr st b) with
        Bool true -> Cmd(Seq(c,While(b,c)),st)
      | Bool false -> St st
      | _ -> failwith "While condition is not boolean!" )
  | _ -> failwith "No match"
