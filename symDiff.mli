
type unop = Log | Sin | Cos

(*maybe I should differentiate between associative and non-associative binops*)
type binop = Add | Sub | Mlt | Div | Pow

type expr =
  | Var
  | Const of float
  | Unop of unop * expr
  | Binop of binop * expr * expr

let rec fold (f_v: unit -> 'a)
             (f_c: float -> 'a)
             (f_u: unop -> 'a -> 'a)
             (f_b: binop -> 'a -> 'a -> 'a)
             (e: expr)
             : 'a =

val fold : (unit -> 'a)
        -> (float -> 'a )
        -> (unop -> 'a -> 'a)
        -> (binop -> 'a -> 'a -> 'a)
        -> expr
        -> 'a

val fold_unop : ('a -> 'a) -> ('a -> 'a) -> ('a -> 'a) -> unop -> 'a -> 'a

val fold_binop : ('a -> 'a)
              -> ('a -> 'a)
              -> ('a -> 'a)
              -> ('a -> 'a)
              -> ('a -> 'a)
              -> binop
              -> 'a
              -> 'a
              -> 'a

val eval : expr -> float

val diff : expr -> expr
