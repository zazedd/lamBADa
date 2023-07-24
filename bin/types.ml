open Ast

exception TypeError of string

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TGen -> "a'"
  | TArrow (t1, t2) -> string_of_typ t1 ^ " -> " ^ string_of_typ t2

let lookup ctx e =
  match List.assoc_opt e ctx with
  | Some t -> t
  | None -> TypeError "Unbound variable" |> raise

let extend ctx name t = (name, t) :: ctx

let rec contains_int_bop = function
  | Int _ | Bool _ | Var _ -> false
  | Bop (bop, _, _) -> ( match bop with Add | Mult -> true | _ -> false)
  | Let (_, _, e) -> contains_int_bop e
  | If (_, e1, e2) -> contains_int_bop e1 || contains_int_bop e2
  | AnonFun (_, e) -> contains_int_bop e
  | App (_, e) -> contains_int_bop e
  | LetIn _ -> assert false

let rec contains_bool_bop = function
  | Int _ | Bool _ | Var _ -> false
  | Bop (bop, _, _) -> ( match bop with Eq -> true | _ -> false)
  | Let (_, _, e) -> contains_bool_bop e
  | If (_, e1, e2) -> contains_bool_bop e1 || contains_bool_bop e2
  | AnonFun (_, e) -> contains_bool_bop e
  | App (_, e) -> contains_bool_bop e
  | LetIn _ -> assert false

let rec typeof ctx = function
  | Int _ -> (TInt, ctx)
  | Bool _ -> (TBool, ctx)
  | Var v -> (lookup ctx v, ctx)
  | Bop (op, e1, e2) -> typeof_bop ctx op e1 e2
  | Let (name, t, e) -> typeof_let ctx name t e
  | LetIn (name, t, e1, e2) -> typeof_letin ctx name t e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  (* NOT COMPLETE I KNOW THIS IS DUMB *)
  | AnonFun (name, e) -> typeof_anonfun ctx name e
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
  | TBool | TGen -> (
      match typeof ctx e2 with
      | t when t = typeof ctx e3 -> t
      | _ -> TypeError "Both if branches must be the same type" |> raise)
  | _ -> TypeError "If guard must be a boolean" |> raise

(* we return ctx and not ctx' because we dont care about the variables defined inside lambda functions *)
(* this is a really stupid way of doing type inference lmao ill fix later *)
and typeof_anonfun ctx name e =
  let is_result_int =
    contains_int_bop e || (fun a -> match a with Int _ -> true | _ -> false) e
  in
  let is_result_bool =
    contains_bool_bop e
    || (fun a -> match a with Bool _ -> true | _ -> false) e
  in
  let res =
    if is_result_int then TInt else if is_result_bool then TBool else TGen
  in
  let ctx' = extend ctx name res in
  let res_body = typeof ctx' e |> fst in
  (TArrow (res, res_body), ctx)

let typechecker ctx e =
  let e', res_t_ctx = typeof ctx e in
  (e, e', res_t_ctx)
