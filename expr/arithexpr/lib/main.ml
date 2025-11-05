open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | And(e1,e2) -> "And("^(string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Or(e1,e2) -> "Or("^(string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e) -> "Not(" ^ (string_of_boolexpr e) ^ ")"
  | Succ(e) -> "Succ(" ^ (string_of_boolexpr e) ^ ")"            
  | Pred(e) -> "Pred(" ^ (string_of_boolexpr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_boolexpr e) ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NatExeption of string
exception TypeError of string
exception NoRuleApplies

type exprval = Bool of bool | Nat of int

let rec is_nv = function
    Zero -> true
  | Succ e -> is_nv e
  | _ -> false
    
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(b,e1,e2) -> If(trace1 b,e1,e2)
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> And(trace1 e1, e2)
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> Or(trace1 e1, e2)
  | Not True -> False
  | Not False -> True
  | Not e -> Not(trace1 e)
  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(e)) when (is_nv e) -> e
  | Pred(Zero) -> raise (NatExeption "Trying pred(0) :(")
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when (is_nv e) -> False 
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If(b,e1,e2) -> (match (eval b) with
        Bool true -> eval e1
      | Bool false -> eval e2
      | _ -> raise (TypeError "Invalid_argument"))
  | And(e1,e2) -> (match (eval e1) with
        Bool true -> eval e2
      | Bool false -> Bool false
      | _ -> raise (TypeError "Invalid_argument"))
  | Or(e1,e2) -> (match (eval e1) with
        Bool true -> Bool true
      | Bool false -> eval e2 
      | _ -> raise (TypeError "Invalid_argument"))
  | Not e -> (match (eval e) with
        Bool true -> Bool false
      | Bool false -> Bool true
      | _ -> raise (TypeError "Invalid_argument"))
  | Succ e -> ( match (eval e) with
        Nat n -> Nat (n + 1)
      | _ -> raise(TypeError "Invalid_argument <Succ(Nat)>") )
  | Pred e -> ( match (eval e) with
        Nat 0 -> raise (NatExeption "Pred(0)")
      | Nat n -> Nat (n-1)
      | _ -> raise(TypeError "Invalid_argument <Pred(Nat)>"))
  | IsZero e -> (match (eval e) with
        Nat 0 -> Bool true
      | Nat _ -> Bool false
      | _ -> raise(TypeError "Invalid_argument"))
