
(*module for symbolic differentiation*)

type expr = 
  | Const of float     (*Const  n       represents the coonstant n*)
  | Var                (*Var            represents the variable x*)
  | Plus  of exp * exp (*Plus  (e1, e2) represents e1+e2*)
  | Minus of exp * exp (*Minus (e1, e2) represents e1-e2*)
  | Mult  of exp * exp (*Mult  (e1, e2) represents e1*e2*)
  | Div   of exp * exp (*Div   (e1, e2) represents e1/e2*)
  | Pow   of exp * exp (*Pow   (e1, e2) represents e1^e2*)
  | Log   of exp       (*Log   e        represents ln(e)*)
  | Sin   of exp       (*Sin   e        represents sin(e)*)
  | Cos   of exp       (*Cos   e        represents cos(e)*)


(*produces a symbolic representation of a string*)
let to_exp (s: string) : expr = 
  failwith "not implemented"

(*simplifies an expr*)
let simp (e: expr) : expr = 
  failwith "not implemented"

(*differentiates an expression*)
let diff (e: expr) : expr = 
  failwith "not implemented"