type bop = Add | Mult | Eq

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Bop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | AnonFun of string * expr
  | App of expr * expr
