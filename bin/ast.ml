type bop = Add | Mult | Eq
type typ = TInt | TBool

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Bop of bop * expr * expr
  | LetIn of string * typ * expr * expr
  | Let of string * typ * expr
  | If of expr * expr * expr
  | AnonFun of string * expr
  | App of expr * expr
