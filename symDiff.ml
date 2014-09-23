
(*module for symbolic differentiation*)

type expr = 
  | Const of float       (*Const  n       represents the coonstant n*)
  | Var                  (*Var            represents the variable x*)
  | Plus  of expr * expr (*Plus  (e1, e2) represents e1+e2*)
  | Minus of expr * expr (*Minus (e1, e2) represents e1-e2*)
  | Mult  of expr * expr (*Mult  (e1, e2) represents e1*e2*)
  | Div   of expr * expr (*Div   (e1, e2) represents e1/e2*)
  | Pow   of expr * expr (*Pow   (e1, e2) represents e1^e2*)
  | Log   of expr        (*Log   e        represents ln(e)*)
  | Sin   of expr        (*Sin   e        represents sin(e)*)
  | Cos   of expr        (*Cos   e        represents cos(e)*)


(*produces a symbolic representation of a string*)
let to_expr (s: string) : expr = 
  failwith "not implemented"


(*simplifies an expr*)
let simp (e: expr) : expr = 
  failwith "not implemented"

(*differentiates an expression*)
let rec diff (e: expr) : expr = 
  match e with
  | Const  n            -> Const 0.0
  | Var                 -> Const 1.0
  | Plus  (e1, e2)      -> Plus  (diff e1, diff e2)
  | Minus (e1, e2)      -> Minus (diff e1, diff e2)
  | Mult  (e1, e2)      -> Plus  (Mult (diff e1, e2), Mult (e2, diff e1))
  | Div   (e1, e2)      -> 
    let numerator   = Minus (Mult (diff e1, e2), Mult (e2, diff e1))
    and denominator = Pow (e2, Const 2.0)
    in Div (numerator, denominator) 
  | Pow   (e1, Const n) -> 
    Mult  (Const n, Mult (diff e1, Pow (e1, Const (n -. 1.0)))) 
  | Pow   (e1, e2)      -> 
    let e1_to_e2        = Pow (e1, e2) 
    and e2'             = diff e2
    and log_e1          = Log (e1)
    and e1'e2_div_by_e1 = Div (Mult (diff e1, e2), e1)
    in Mult (e1_to_e2, Mult (e2', Mult (log_e1, e1'e2_div_by_e1))) 
  | Log    e            -> Div   (diff e, e) 
  | Sin    e            -> Mult  (diff e, Cos e)
  | Cos    e            -> Mult  (diff e, Mult (Const (-1.0), Sin e))

(*returns a string representing [e]*)
let to_string (e: expr) : string =
  failwith "not implemented"

(*also returns a string representing [e], but with parenthesis indicating the 
  structure of the expression*)
let rec lispish_to_string (e: expr) : string =
  match e with
  | Const  n       -> string_of_float n
  | Var            -> "x"
  | Plus  (e1, e2) -> "(" ^ (lispish_to_string e1) ^ ") + (" 
                      ^ (lispish_to_string e2) ^ ")" 
  | Minus (e1, e2) -> "(" ^ (lispish_to_string e1) ^ ") - (" 
                      ^ (lispish_to_string e2) ^ ")" 
  | Mult  (e1, e2) -> "(" ^ (lispish_to_string e1) ^ ") * (" 
                      ^ (lispish_to_string e2) ^ ")" 
  | Div   (e1, e2) -> "(" ^ (lispish_to_string e1) ^ ") / (" 
                      ^ (lispish_to_string e2) ^ ")" 
  | Pow   (e1, e2) -> "(" ^ (lispish_to_string e1) ^ ") ^ (" 
                      ^ (lispish_to_string e2) ^ ")" 
  | Log    e       -> "ln("  ^ (lispish_to_string e) ^ ")"
  | Sin    e       -> "sin(" ^ (lispish_to_string e) ^ ")"
  | Cos    e       -> "cos(" ^ (lispish_to_string e) ^ ")"
