type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

exception NatExeption of string
exception TypeError of string
exception NoRuleApplies


type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT
