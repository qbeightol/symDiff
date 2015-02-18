
(******************************************************************************)
(* Type Declarations                                                          *)
(******************************************************************************)

type atom = Var | Const of float
type uop  = Log | Sin | Cos
type bop  = Add | Sub | Mlt | Div | Pow

type expr =
  | Atom of atom
  | Uop of uop * expr
  | Bop of bop * expr * expr

type precedence = Add_Sub | Mlt_Div | Exp | Atom_Uop

(******************************************************************************)
(* Folding                                                                    *)
(******************************************************************************)

(* move to an .mli:
let rec fold (f_atom: atom -> 'a)
             (f_uop: uop -> 'a -> 'a)
             (f_bop: bop -> 'a -> 'a -> 'a)
             (e: expr)
             : a =
*)
let rec fold f_atom f_uop f_bop e =
  let fold_w_fs = fold f_atom f_uop f_bop in
  match e with
  | Atom a -> f_atom a
  | Uop (u,e) -> f_uop u (fold_w_fs e)
  | Bop (b,e1,e2) -> f_bop b (fold_w_fs e1) (fold_w_fs e2)

(*creates a function for processing atoms using [f_var], a function that
  describes how to process a variable, and [f_const] which describes how to
  process the floats stored in constants *)
let fold_atom f_var f_const = function
  | Var -> f_var ()
  | Const n -> f_const n

(*used to create a function for processing uops given functions for processing
  logs, sines, and cosines*)
let fold_uop f_log f_sin f_cos = function
  | Log -> f_log
  | Sin -> f_sin
  | Cos -> f_cos

(*makes a function for processing bops given functions for processing additions,
  subtractions, multiplications, divisions, and powers*)
let fold_bop f_a f_s f_m f_d f_p = function
  | Add -> f_a
  | Sub -> f_s
  | Mlt -> f_m
  | Div -> f_d
  | Pow -> f_p

(*like fold, but also allows f_uop and f_bop to access an operators
  subexpressions directly instead of only providing access through accumulators
  (useful for implementing diff) *)
let rec fold' (f_atom: atom -> 'a)
              (f_uop: uop -> expr -> 'a -> 'a)
              (f_bop: bop -> expr -> expr -> 'a -> 'a -> 'a)
              (e: expr)
              : 'a =
  let fold_w_fs = fold' f_atom f_uop f_bop in
  match e with
  | Atom a -> f_atom a
  | Uop (u,e) -> f_uop u e (fold_w_fs e)
  | Bop (b,e1,e2) -> f_bop b e1 e2 (fold_w_fs e1) (fold_w_fs e2)

(*note: there's no need to create new functions like fold'_uop or fold'_bop--the
  original functions will work with functions with type 'a -> 'a as well as
  functions with type expr -> 'a -> 'a *)

(******************************************************************************)
(* Utility Functions & Pseudo-Constructors                                    *)
(******************************************************************************)

let id x = x
let wrap f _ = f

(*I'm going to overwrite log, sin, and cos, so I'm creating the functions to
  access the original defintions of log, sin and cos*)
let log' = log
let sin' = sin
let cos' = cos

let var = Atom Var
let const n = Atom (Const n)
let log e = Uop (Log, e)
let sin e = Uop (Sin, e)
let cos e = Uop (Cos, e)
let add e1 e2 = Bop (Add, e1, e2)
let sub e1 e2 = Bop (Sub, e1, e2)
let mlt e1 e2 = Bop (Mlt, e1, e2)
let div e1 e2 = Bop (Div, e1, e2)
let pow e1 e2 = Bop (Pow, e1, e2)

let (+) = add
let (-) = sub
let ( * ) = mlt
let (/) = div
let (^^) = pow

let x = var
let neg_one = const (-1.0)
let zero = const 0.0
let one = const 1.0
let two = const 2.0

let is_atom = function Atom _ -> true | _ -> false
let is_uop  = function Uop  _ -> true | _ -> false
let is_bop  = function Bop  _ -> true | _ -> false

(*checks whether an expression contains at least one instance of Var*)
let contains_var_atom = fold_atom (fun () -> true) (fun _ -> false)

let contains_var = fold contains_var_atom (wrap id) (wrap (||))

let bop_precedence = function
  | Add | Sub -> Add_Sub
  | Mlt | Div -> Mlt_Div
  | Pow -> Exp

let precedence = function
  | Atom _ | Uop _ -> Atom_Uop
  | Bop (bop,_,_) -> bop_precedence bop

(******************************************************************************)
(* Evaluation Functions                                                       *)
(******************************************************************************)

let eval_atom =
  fold_atom (fun () -> failwith "expression contains variables") id

let eval_uop = fold_uop log' sin' cos'

let eval_bop = fold_bop (+.) (-.) ( *. ) (/.) ( ** )

let eval = fold eval_atom eval_uop eval_bop

(******************************************************************************)
(* String Conversion Functions                                                *)
(******************************************************************************)
let string_of_atom = fold_atom (fun () -> "x") string_of_float

(*creates a string from a single uop, avoiding doubling of parentheses
  ex: the expr representing log(x*x) would be printed Log((x * x)) if we didn't
  check for binops in the sub-expression.*)
let sou uop_name e e_str =
  if is_bop e then uop_name ^ e_str
  else uop_name ^ "(" ^ e_str ^ ")"

let sob bop_sym _ _ e1_str e2_str =
  "(" ^ e1_str ^ " " ^ bop_sym ^ " " ^ e2_str ^ ")"

let string_of_uop = fold_uop (sou "log") (sou "sin") (sou "cos")
let string_of_bop = fold_bop (sob "+") (sob "-") (sob "*") (sob "/") (sob "^")

(*a fully parenthesized representation of expr--kinda lispish*)
let string_of_expr = fold' string_of_atom string_of_uop string_of_bop

let sou' uop_name e e_str = uop_name ^ "(" ^ e_str ^ ")"

(*parenthesizes an expression, [e] based off the precedences of the bop and the
  sub-expression as well as rule given by [cmp]*)
let paren cmp e_prec e_str bop_prec =
  if cmp e_prec bop_prec then "(" ^ e_str ^ ")" else e_str

(*for associative bops*)
let sob'_a bop_sym bop_prec e1 e2 e1_str e2_str =
  let fst_str = paren (<)  (precedence e1) e1_str bop_prec
  and snd_str = paren (<) (precedence e2) e2_str bop_prec
  in fst_str ^ " " ^ bop_sym ^ " " ^ snd_str

(*for non-associatve bops*)
let sob'_n bop_sym bop_prec e1 e2 e1_str e2_str =
  let fst_str = paren (<)  (precedence e1) e1_str bop_prec
  and snd_str = paren (<=) (precedence e2) e2_str bop_prec
  in fst_str ^ " " ^ bop_sym ^ " " ^ snd_str

let str_of_uop = fold_uop (sou' "log") (sou' "sin") (sou' "cos")
let str_of_bop =
  let a = (sob'_a "+" Add_Sub)
  and s = (sob'_n "-" Add_Sub)
  and m = (sob'_a "*" Mlt_Div)
  and d = (sob'_n "/" Mlt_Div)
  and p = (sob'_a "^" Exp)
  in fold_bop a s m d p

let str_of_expr = fold' string_of_atom str_of_uop str_of_bop

(******************************************************************************)
(* Differentiation Functions                                                  *)
(******************************************************************************)

let diff_atom = fold_atom (wrap one) (wrap zero)

(*differentiates log(f(x)) given f(x) and f'(x)*)
let diff_log f f' = f' / f

(*differentiates sin(f(x)) given f(x) and f'(x)*)
let diff_sin f f' = f' * cos f

(*differentiates cos(f(x)) given f(x) and f'(x)*)
let diff_cos f f' = f' * neg_one * sin f

let diff_uop = fold_uop diff_log diff_sin diff_cos

(*differentiates f(x) + g(x) given f(x), f'(x), g(x), and g'(x)*)
let diff_add _ _ f' g' = f' + g'

(*differentiates f(x) - g(x) given f(x), f'(x), g(x), and g'(x)*)
let diff_sub _ _ f' g' = f' - g'

(*differentiates f(x) * g(x) given f(x), f'(x), g(x), and g'(x)*)
let diff_mlt f g f' g' = (f' * g) + (f * g')

(*differentiates f(x) / g(x) given f(x), f'(x), g(x), and g'(x)*)
let diff_div f g f' g' = ((f' * g) - (f * g')) / (g ^^ two)

(*differentiates f(x) ^ g(x) given f(x), f'(x), g(x), and g'(x)*)
let diff_pow f g f' g' =
  if not (contains_var g) then
    let n = eval g in
    const n * f' * (f ^^ const (n -. 1.0))
  else ((f ^^ g) * g' * log f) + f' * g / f

let diff_binop = fold_bop diff_add diff_sub diff_mlt diff_div diff_pow

let diff = fold' diff_atom diff_uop diff_binop
