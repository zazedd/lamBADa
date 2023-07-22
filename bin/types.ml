open Ast

exception TypeError of string

let lookup ctx e =
  match List.assoc_opt e ctx with
  | Some t -> t
  | None -> TypeError "Unbound variable" |> raise

let extend ctx name t = (name, t) :: ctx

let rec typeof ctx = function
  | Int _ -> (TInt, ctx)
  | Bool _ -> (TBool, ctx)
  | Var v -> (lookup ctx v, ctx)
  | Bop (op, e1, e2) -> typeof_bop ctx op e1 e2
  | Let (name, t, e) -> typeof_let ctx name t e
  | LetIn (name, t, e1, e2) -> typeof_letin ctx name t e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  (* NOT COMPLETE I KNOW THIS IS DUMB *)
  | AnonFun (_, e) -> typeof ctx e
  | App (_, e) -> typeof ctx e

and typeof_bop ctx op e1 e2 =
  match (op, typeof ctx e1 |> fst, typeof ctx e2 |> fst) with
  | Add, TInt, TInt -> (TInt, ctx)
  | Mult, TInt, TInt -> (TInt, ctx)
  | Eq, TInt, TInt -> (TBool, ctx)
  | _ -> TypeError "Operator requires the same type on both sides" |> raise

and typeof_let ctx name t e =
  let t' = typeof ctx e |> fst in
  if t = t' then (t, extend ctx name t')
  else TypeError "Definition does not match type" |> raise

and typeof_letin ctx name t e1 e2 =
  let t' = typeof ctx e1 |> fst in
  if t = t' then
    let ctx' = extend ctx name t' in
    typeof ctx' e2
  else TypeError "Definition does not match type" |> raise

and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 |> fst with
  | TBool -> (
      match typeof ctx e2 with
      | t when t = typeof ctx e3 -> t
      | _ -> TypeError "Both if branches must be the same type" |> raise)
  | _ -> TypeError "If guard must be a boolean" |> raise

let typechecker ctx e =
  let _, res_t_ctx = typeof ctx e in
  (e, res_t_ctx)
