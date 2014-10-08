
type expr =
  | Var
  | Const of float
  | Log of expr
  | Sin of expr
  | Cos of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mlt of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

(******************************************************************************)
(* Folding functions **********************************************************)
(******************************************************************************)
let rec fold (f_atom: expr -> 'a)
             (f_uop: expr -> 'a -> 'a)
             (f_bop: expr -> 'a -> 'a -> 'a)
             (e: expr)
             : 'a =
  match e with
  | Var | Const _ -> f_atom e
  | Log f | Sin f | Cos f -> f_uop e (fold f_atom f_uop f_bop f)
  | Add (f,g) | Sub (f,g) | Mlt (f,g) | Div (f,g) | Pow (f,g)
    -> f_bop e (fold f_atom f_uop f_bop f) (fold f_atom f_uop f_bop g)

let rec fold_atom f_var f_const = function
  | Var -> f_var ()
  | Const n -> f_const n
  | _ -> failwith "not an atom"

let rec fold_uop f_log f_sin f_cos e acc =
  match e with
  | Log f -> f_log f acc
  | Sin f -> f_sin f acc
  | Cos f -> f_cos f acc
  | _ -> failwith "not a unary operator"

let rec fold_bop f_add f_sub f_mlt f_div f_pow e acc1 acc2 =
  match e with
  | Add (f,g) -> f_add f g acc1 acc2
  | Sub (f,g) -> f_sub f g acc1 acc2
  | Mlt (f,g) -> f_mlt f g acc1 acc2
  | Div (f,g) -> f_div f g acc1 acc2
  | Pow (f,g) -> f_pow f g acc1 acc2
  | _ -> failwith "not a binary operator"

let wrapl1 x = fun _ -> x
let wrapl2 x = fun _ _ -> x

let wrapr1 _ x = x

let fold_uop' f_l f_s f_c = fold_uop (wrapl1 f_l) (wrapl1 f_s) (wrapl1 f_c)

let fold_bop' f_a f_s f_m f_d f_p =
  fold_bop (wrapl2 f_a) (wrapl2 f_s) (wrapl2 f_m) (wrapl2 f_d) (wrapl2 f_p)

(******************************************************************************)
(* Utility Functions **********************************************************)
(******************************************************************************)
let is_atom = function Var | Const _ -> true | _ -> false

let is_uop = function Log _ | Sin _ | Cos _ -> true | _ -> false

let is_bop = function Add _ | Sub _ | Mlt _ | Div _ | Pow _ -> true | _ -> false

(*checks whether an expression contains at least one instance of Var*)
let contains_var_atom = fold_atom (fun () -> true) (wrapl1 false)

let contains_var = fold contains_var_atom wrapr1 (wrapl1 (||))

(******************************************************************************)
(* Evaluation Functions *******************************************************)
(******************************************************************************)

let eval_atom =
  fold_atom (fun () -> failwith "expression contains variables") C.id

let eval_uop = fold_uop' log sin cos

let eval_bop = fold_bop' (+.) (-.) ( *. ) (/.) ( ** )

let eval = fold eval_atom eval_uop eval_bop

(******************************************************************************)
(* Conversion Functions *******************************************************)
(******************************************************************************)
let string_of_atom = fold_atom (fun () -> "x") string_of_float

(*creates a string from a single uop, avoiding unnecessary parentheses*)
let sou uop_name e e_str =
  if is_bop e then uop_name ^ e_str
  else uop_name ^ "(" ^ e_str ^ ")"

let sob bop_sym _ _ e1_str e2_str =
  "(" ^ e1_str ^ " " ^ bop_sym ^ " " ^ e2_str ^ ")"

let string_of_uop = fold_uop (sou "log") (sou "sin") (sou "cos")
let string_of_bop = fold_bop (sob "+") (sob "-") (sob "*") (sob "/") (sob "^")
let string_of_expr = fold string_of_atom string_of_uop string_of_bop

(******************************************************************************)
(* Differentiation Functions **************************************************)
(******************************************************************************)

let diff_atom = function
  | Var -> Const 1.0
  | Const _ -> Const 0.0
  | _ -> failwith "not an atom"

let diff_uop e f' =
  match e with
  | Log f -> Div (f', f)
  | Sin f -> Mlt (f', Cos f)
  | Cos f -> Mlt (f', Mlt (Const (-1.0), Sin f))
  | _ -> failwith "not a unary operator"

let diff_binop e f' g' =
  match e with
  | Add (f,g) -> Add (f', g')
  | Sub (f,g) -> Sub (f', g')
  | Mlt (f,g) -> Add (Mlt (f', g), Mlt (f, g'))
  | Div (f,g)
    -> let numerator = Sub (Mlt (f', g), Mlt (f, g'))
       and denominator = Pow (g, Const 2.0)
       in Div (numerator, denominator)
  | Pow (f,g) when not (contains_var g)
    -> let n = eval g
       in Mlt (Const n, Mlt (f', Pow (f, Const (n -. 1.0))))
  | Pow (f,g) ->
    (*(f(x)^g(x))*g'(x)ln(f(x)) + f'(x)g(x)/f(x)*)
    Add (Mlt (Pow (f,g), Mlt (g', Log f)), Div (Mlt (f', g), f))
  | _ -> failwith "not a binary operator"

let diff = fold diff_atom diff_uop diff_binop

