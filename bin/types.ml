open Ast

exception TypeError of string

let rec string_of_typ = function
  | TInt | TVar { def = Some TInt; _ } -> "int"
  | TBool | TVar { def = Some TBool; _ } -> "bool"
  | TVar { id; _ } -> Format.sprintf "%c" (Char.chr (id + Char.code 'a'))
  | TArrow (t1, t2) -> string_of_typ t1 ^ " -> " ^ string_of_typ t2

let last_id = ref 0
(* let next_general_name = ref "a" *)

let new_var () =
  let newv = TVar { id = !last_id; def = None } in
  last_id := !last_id + 1;
  newv

(* any variable coming out this function is TVar { def = None; _}, keep that in mind *)
let rec instanciate = function
  | TVar { def = Some t; _ } -> instanciate t
  | t -> t

let rec free_vars t =
  match instanciate t with
  | TVar v -> [ v ]
  | TArrow (t1, t2) ->
      let l1 : tvar list = free_vars t1 in
      let l2 : tvar list = free_vars t2 in
      List.fold_left
        (fun acc a -> if List.mem a acc then acc else a :: acc)
        l2 l1
  | _ -> []

let lookup ctx name =
  try
    let _, t = List.find (fun (ss, _) -> ss = name) (fst ctx) in
    let s = List.fold_left (fun acc a -> (a, new_var ()) :: acc) [] (snd ctx) in
    let rec subst t =
      match instanciate t with
      | TVar x as t -> (
          try List.find (fun (v, _) -> x = v) s |> snd with Not_found -> t)
      | TArrow (t1, t2) -> TArrow (subst t1, subst t2)
      | t -> t
    in
    subst t
  with Not_found -> failwith (Format.sprintf "Unbound variable %s" name)

let rec unify t1 t2 =
  match (instanciate t1, instanciate t2) with
  | TVar { id = id1; _ }, TVar { id = id2; _ } when id1 = id2 -> ()
  | TArrow (t11, t12), TArrow (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | TVar v, t -> v.def <- Some t
  | t, TVar v -> v.def <- Some t
  | t1, t2 when t1 = t2 -> ()
  | t1, t2 ->
      failwith
        (Format.sprintf "Cant unify %s with %s." (string_of_typ t1)
           (string_of_typ t2))

let extend ctx name t = ((name, instanciate t) :: fst ctx, free_vars t @ snd ctx)

let rec typeof ctx = function
  | Int _ -> (TInt, ctx)
  | Bool _ -> (TBool, ctx)
  | Var name -> (lookup ctx name, ctx)
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  | Bop (op, e1, e2) -> typeof_bop ctx op e1 e2
  | Let (name, t, e) -> typeof_let ctx name t e
  | LetIn (name, t, e1, e2) -> typeof_letin ctx name t e1 e2
  | AnonFun (v, e) -> typeof_anonfun ctx v e
  | App (e1, e2) -> typeof_app ctx e1 e2

and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 |> fst |> instanciate with
  | TBool -> if_branch ctx e2 e3
  | TVar v ->
      let _ = v.def <- Some TBool in
      if_branch ctx e2 e3
  | _ -> TypeError "If guard must be a boolean" |> raise

and if_branch ctx e2 e3 =
  match (typeof ctx e2 |> fst, typeof ctx e3 |> fst) with
  | TVar v, t ->
      let _ = v.def <- Some t in
      (t, ctx)
  | t, TVar v ->
      let _ = v.def <- Some t in
      (t, ctx)
  | t1, t2 when t1 = t2 -> (t1, ctx)
  | _ -> TypeError "Both if branches must be the same type" |> raise

and typeof_bop ctx op e1 e2 =
  match
    ( op,
      typeof ctx e1 |> fst |> instanciate,
      typeof ctx e2 |> fst |> instanciate )
  with
  | Add, TInt, TInt | Mult, TInt, TInt -> (TInt, ctx)
  | Eq, TInt, TInt -> (TBool, ctx)
  | Eq, TVar v, TInt ->
      let () = v.def <- Some TInt in
      (TBool, ctx)
  | Eq, TInt, TVar v ->
      let () = v.def <- Some TInt in
      (TBool, ctx)
  | Eq, TVar v1, TVar v2 ->
      let () = v1.def <- Some TInt in
      let () = v2.def <- Some TInt in
      (TBool, ctx)
  | _, TVar v, TInt ->
      let () = v.def <- Some TInt in
      (TInt, ctx)
  | _, TInt, TVar v ->
      let () = v.def <- Some TInt in
      (TInt, ctx)
  | _, TVar v1, TVar v2 ->
      let () = v1.def <- Some TInt in
      let () = v2.def <- Some TInt in
      (TInt, ctx)
  | _ -> TypeError "Operator requires the same type on both sides" |> raise

and typeof_let ctx name t e =
  let t' = typeof ctx e |> fst in
  if t = t' then (t, extend ctx name t')
  else TypeError "Definition does not match type" |> raise

and typeof_letin ctx name t e1 e2 =
  let t1 = typeof ctx e1 |> fst in
  if t <> t1 then TypeError "Types in let expression do not match" |> raise
  else
    let ctx' = extend ctx name t1 in
    typeof ctx' e2

and typeof_anonfun ctx v e =
  let t = new_var () in
  let ctx' = ((v, t) :: fst ctx, snd ctx) in
  (TArrow (t, typeof ctx' e |> fst), ctx')

and typeof_app ctx e1 e2 =
  let t1 = typeof ctx e1 |> fst in
  let t2 = typeof ctx e2 |> fst in
  let t = new_var () in
  unify t1 (TArrow (t2, t));
  (instanciate t, ctx)

let typechecker ctx e =
  let e', res_t_ctx = typeof ctx e in
  (e, e', res_t_ctx)
