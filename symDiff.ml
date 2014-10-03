
(*module for symbolic differentiation*)

type unop = Log | Sin | Cos

(*maybe I should differentiate between associative and non-associative binops*)
type binop = Add | Sub | Mlt | Div

type expr = 
  | Const of float
  | Var
  | Unop of unop * expr
  | Binop of binop * expr * expr





(*produces a symbolic representation of a string*)
let to_expr (s: string) : expr = 
  failwith "not implemented"


(*simplifies an expr*)
let rec simp (e: expr) : expr = 
  (*idea: code in a bunch of identities & simplification rules and apply them 
    repeatedly until the expression can't be reduced any more: i.e. return
    a fixed point of simp using e as a seed. Note: may be ineffecient, but
    the method models how humans actually approach simplification*)
  match e with 
  | Plus  (Const 0., e)
  | Plus  (e, Const 0.)
  | Minus (e, Const 0.) 
  | Mult  (Const 1., e)
  | Mult  (e, Const 1.)
  | Div   (e, Const 1.) 
  | Pow   (e, Const 1.) -> simp e
  | Pow   (Const real_e, Log (e)) 
  | Log   (Pow (Const real_e, e)) when real_e = exp 1. -> simp e 
  | Plus  (Const n1, Const n2) -> Const (n1 +. n2)
  | Minus (Const n1, Const n2) -> Const (n1 -. n2)
  | Mult  (Const n1, Const n2) -> Const (n1 *. n2)
  | Div   (Const n1, Const n2) when n2 <> 0. -> Const (n1 /. n2)
  | Pow   (Const n1, Const n2) -> Const (n1 ** n2)
  | Log   (Const n) -> Const (log n)
  | Sin   (Const n) -> Const (sin n)
  | Cos   (Const n) -> Const (cos n)
  | Mult  (Const n, Plus (e1,e2)) -> Plus (Mult (Const n, e1), Mult (Const n, e2))
  | Mult (Pow (Var, p1), Pow (Var, p2)) -> Pow (Var, simp (Mult (p1, p2)))
  | Pow (Pow (e1, Const n1), Const n2) -> Pow (simp e1, Const (n1*.n2))

  | Div   (Div (e1, e2), e3) -> Div (e1, Mult (e2, e3))
  | Div   (e1, Div (e2, e3)) -> Div (Mult (e1, e3), e2)

  | Plus  (Log e1, Log e2) -> Log (Mult (e1,e2))
  | Minus (Log e1, Log e2) -> Log (Div  (e1,e2))
  | Mult  (e1,     Log e2) -> Log (Pow  (e2,e1)) 
    (*maybe I should only apply the last rule when e1 is a constant*)

  | Plus  (e1, e2)      -> Plus  (simp e1, simp e2)
  | Minus (e1, e2)      -> Minus (simp e1, simp e2)
  | Mult  (e1, e2)      -> Mult  (simp e1, simp e2)
  | Div   (e1, e2)      -> Div   (simp e1, simp e2)
  | Pow   (e1, e2)      -> Pow   (simp e1, simp e2)
  | Log    e            -> Log   (simp e)
  | Sin    e            -> Sin   (simp e)
  | Cos    e            -> Cos   (simp e)
  | _ -> e

let rec simp' e = let e' = simp e in if e' = e then e' else simp' e'

let rec fix_point f seed = 
  let seed' = f seed in if seed' = seed then  seed' else fix_point f seed'

let rec fix_point' f seed =
  let seed' = f seed in 
    if abs_float (seed' -. seed) < 10. ** (-4.) then seed' 
    else fix_point' f seed'


let rec simp2 (e: expr) : expr =
  match e with 
  (*check for e.g., additive, multiplicative identities*)
  | Plus  (Const 0., e)
  | Plus  (e, Const 0.)
  | Minus (e, Const 0.) 
  | Mult  (Const 1., e)
  | Mult  (e, Const 1.)
  | Div   (e, Const 1.) 
  | Pow   (e, Const 1.) -> simp e
  | Pow   (Const real_e, Log (e)) 
  | Log   (Pow (Const real_e, e)) when real_e = exp 1. -> simp e 
  (*combine fractions*)
  | Mult  (Div (a,b), Div (c,d)) -> Div (Mult (a,c), Mult (b,d))
  (*expand fractions*)
  | Mult  (a, Div (b,c))
  | Mult  (Div (b,c), a) -> Div (Mult (a,b), c)
  | Div   (Div (a,b), c) -> Div (a, Mult (b,c))
  | Div   (a, Div (b,c)) -> Div (Mult (a,c), b)
  (*inception*)
  | Plus  (e1, e2)      -> Plus  (simp2 e1, simp2 e2)
  | Minus (e1, e2)      -> Minus (simp2 e1, simp2 e2)
  | Mult  (e1, e2)      -> Mult  (simp2 e1, simp2 e2)
  | Div   (e1, e2)      -> Div   (simp2 e1, simp2 e2)
  | Pow   (e1, e2)      -> Pow   (simp2 e1, simp2 e2)
  | Log    e            -> Log   (simp2 e)
  | Sin    e            -> Sin   (simp2 e)
  | Cos    e            -> Cos   (simp2 e)
  | _ -> e

  (*I might want to add some rules that push constants to the left*)
  (*I also might want to have rules that recursively simplify all expressions*)

(*differentiates an expression*)

let rec diff (e: expr) : expr = 
  (*note: all of these rules can be derived using the linearity of 
    differentiation, as well as the product, chain, and power rules*)
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

(*walks through the process of differentiating an expression*)
let explain (e: expr) : expr = 
  failwith "not implemented"

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


(*test expressions:*)

(*3ln(x) - 4ln(x+3) + ln(x)*)
let e = Plus (Mult (Const 3.0, Log Var), 
             (Minus (Mult (Const 4.0, Log (Plus (Var, Const 3.0))), 
                     Log (Var))))

let f x = x ** 2. -. 3. *. x +. 4. 
