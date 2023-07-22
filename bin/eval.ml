open Ast
module Ctx = Map.Make (String)

type ctx = value Ctx.t
and value = VInt of int | VBool of bool | Closure of string * expr * ctx

exception RuntimeError of string

let string_of_val = function
  | VInt i -> "- : int = " ^ string_of_int i
  | VBool b -> "- : bool = " ^ string_of_bool b
  | Closure _ -> failwith "Not a value"

let rec eval ctx = function
  | Int v -> (VInt v, ctx)
  | Bool v -> (VBool v, ctx)
  | Var v -> (
      try (Ctx.find v ctx, ctx)
      with Not_found -> RuntimeError "Unbound variable" |> raise)
  | Bop (op, e1, e2) -> eval_bop ctx op e1 e2
  | Let (name, _, e) -> eval_let ctx name e
  | LetIn (name, _, e1, e2) -> eval_letin ctx name e1 e2
  | If (e1, e2, e3) -> eval_if ctx e1 e2 e3
  | AnonFun (v, e) -> (Closure (v, e, ctx), ctx)
  | App (e1, e2) -> eval_app ctx e1 e2

and eval_bop ctx op e1 e2 =
  match (op, eval ctx e1 |> fst, eval ctx e2 |> fst) with
  | Add, VInt a, VInt b -> (VInt (a + b), ctx)
  | Mult, VInt a, VInt b -> (VInt (a * b), ctx)
  | Eq, VInt a, VInt b -> (VBool (a = b), ctx)
  | _ -> RuntimeError "Operator requires the same type on both sides" |> raise

and eval_let ctx name e =
  let v1, newctx = eval ctx e in
  let ctx' = Ctx.add name v1 newctx in
  (v1, ctx')

and eval_letin ctx name e1 e2 =
  let v1, newctx = eval ctx e1 in
  let ctx' = Ctx.add name v1 newctx in
  eval ctx' e2

and eval_if ctx e1 e2 e3 =
  let e, ctx' = eval ctx e1 in
  match e with
  | VBool true -> eval ctx' e2
  | VBool false -> eval ctx' e3
  | _ -> RuntimeError "If guard must be a boolean" |> raise

and eval_app ctx e1 e2 =
  let e, ctx' = eval ctx e1 in
  match e with
  | Closure (v, e, closed_ctx) ->
      let v2 = eval ctx' e2 |> fst in
      let body_env = Ctx.add v v2 closed_ctx in
      eval body_env e
  | _ -> RuntimeError "First parameter of application not a function" |> raise
