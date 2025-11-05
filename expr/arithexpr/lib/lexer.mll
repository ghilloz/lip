{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "zero" { ZERO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "is_zero" { IS_ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | eof { EOF }
