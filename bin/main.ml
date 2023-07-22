open Ast
module Ctx = Map.Make (String)

let empty_ctx = Ctx.empty
let empty_t_ctx = []

type ctx = value Ctx.t
and value = VInt of int | VBool of bool | Closure of string * expr * ctx

exception TypeError of string
exception RuntimeError of string

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lookup ctx e =
  match List.assoc_opt e ctx with
  | Some t -> t
  | None -> TypeError "Unbound variable" |> raise

let extend ctx name t = (name, t) :: ctx

let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var v -> lookup ctx v
  | Bop (op, e1, e2) -> typeof_bop ctx op e1 e2
  | Let (name, t, e1, e2) -> typeof_let ctx name t e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  (* NOT COMPLETE I KNOW THIS IS DUMB *)
  | AnonFun (_, e) -> typeof ctx e
  | App (_, e) -> typeof ctx e

and typeof_bop ctx op e1 e2 =
  match (op, typeof ctx e1, typeof ctx e2) with
  | Add, TInt, TInt -> TInt
  | Mult, TInt, TInt -> TInt
  | Eq, TInt, TInt -> TBool
  | _ -> TypeError "Operator requires the same type on both sides" |> raise

and typeof_let ctx name t e1 e2 =
  let t' = typeof ctx e1 in
  if t = t' then
    let ctx' = extend ctx name t' in
    typeof ctx' e2
  else TypeError "Definition does not match type" |> raise

and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 with
  | TBool -> (
      match typeof ctx e2 with
      | t when t = typeof ctx e3 -> t
      | _ -> TypeError "Both if branches must be the same type" |> raise)
  | _ -> TypeError "If guard must be a boolean" |> raise

let typechecker e =
  let _ = typeof empty_t_ctx e in
  e

let string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | Closure _ -> failwith "Not a value"

let rec eval ctx = function
  | Int v -> VInt v
  | Bool v -> VBool v
  | Var v -> (
      try Ctx.find v ctx
      with Not_found -> TypeError "Unbound variable" |> raise)
  | Bop (op, e1, e2) -> eval_bop ctx op e1 e2
  | Let (name, _, e1, e2) -> eval_let ctx name e1 e2
  | If (e1, e2, e3) -> eval_if ctx e1 e2 e3
  | AnonFun (v, e) -> Closure (v, e, ctx)
  | App (e1, e2) -> eval_app ctx e1 e2

and eval_bop ctx op e1 e2 =
  match (op, eval ctx e1, eval ctx e2) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | _ -> RuntimeError "Operator requires the same type on both sides" |> raise

and eval_let ctx name e1 e2 =
  let v1 = eval ctx e1 in
  let ctx' = Ctx.add name v1 ctx in
  eval ctx' e2

and eval_if ctx e1 e2 e3 =
  match eval ctx e1 with
  | VBool true -> eval ctx e2
  | VBool false -> eval ctx e3
  | _ -> RuntimeError "If guard must be a boolean" |> raise

and eval_app ctx e1 e2 =
  match eval ctx e1 with
  | Closure (v, e, closed_ctx) ->
      let v2 = eval ctx e2 in
      let body_env = Ctx.add v v2 closed_ctx in
      eval body_env e
  | _ -> RuntimeError "First parameter of application not a function" |> raise

let interp s = s |> parse |> typechecker |> eval empty_ctx |> string_of_val

let rec loop () =
  interp (read_line ()) ^ "\n" |> print_string;
  loop ()

let _ = loop ()
