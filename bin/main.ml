open Ast
module Ctx = Map.Make (String)

let empty_ctx = Ctx.empty

type ctx = value Ctx.t
and value = VInt of int | VBool of bool | Closure of string * expr * ctx

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | Closure _ -> failwith "Not a value"

let rec eval ctx = function
  | Int v -> VInt v
  | Bool v -> VBool v
  | Var v -> (
      try Ctx.find v ctx with Not_found -> failwith "Unbound variable")
  | Bop (op, e1, e2) -> eval_bop ctx op e1 e2
  | Let (name, e1, e2) -> eval_let ctx name e1 e2
  | If (e1, e2, e3) -> eval_if ctx e1 e2 e3
  | AnonFun (v, e) -> Closure (v, e, ctx)
  | App (e1, e2) -> eval_app ctx e1 e2

and eval_bop ctx op e1 e2 =
  match (op, eval ctx e1, eval ctx e2) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | _ -> assert false

and eval_let ctx name e1 e2 =
  let v1 = eval ctx e1 in
  let ctx' = Ctx.add name v1 ctx in
  eval ctx' e2

and eval_if ctx e1 e2 e3 =
  match eval ctx e1 with
  | VBool true -> eval ctx e2
  | VBool false -> eval ctx e3
  | _ -> assert false

and eval_app ctx e1 e2 =
  match eval ctx e1 with
  | Closure (v, e, closed_ctx) ->
      let v2 = eval ctx e2 in
      let body_env = Ctx.add v v2 closed_ctx in
      eval body_env e
  | _ -> failwith "First parameter of application not a function"

let interp s = s |> parse |> eval empty_ctx |> string_of_val
let () = interp (read_line ()) ^ "\n" |> print_string
