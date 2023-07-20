type t = Tunit | Tproduct of t * t | Tarrow of t * t
type var = string

module Ctx = Map.Make(String)

type term =
  | Vunit
  | Vvar of var
  | Vproduct of term * term
  | Vfun of var * t * term
  | Vapp of term * term

let rec print_type = function
  | Tunit -> "unit"
  | Tproduct (t1, t2) -> "(" ^ print_type t1 ^ ", " ^ print_type t2 ^ ")"
  | Tarrow (t1, t2) -> "(" ^ print_type t1 ^ " -> " ^ print_type t2 ^ ")"

let rec infer ctx = function
  | Vunit -> Tunit
  | Vvar s -> (
      try Ctx.find s ctx
      with _ -> failwith (Format.sprintf "Variable %s not found" s))
  | Vproduct (t1, t2) -> Tproduct (infer ctx t1, infer ctx t2)
  | Vfun (t1, typ, t2) ->
      Tarrow (typ, infer (Ctx.add t1 typ ctx) t2)
  | Vapp (t1, t2) -> (
      match infer ctx t1 with
      | Tarrow (t1, t12) ->
          let t2 = infer ctx t2 in
          if t1 = t2 then t12
          else
            let () =
              Format.eprintf "Expected %s but got %s.\n" (print_type t1)
                (print_type t2)
            in
            exit 1
      | _ -> failwith "Not a function")

let empty_ctx = Ctx.empty
let id = Vfun ("x", Tunit, Vvar "x")
let () = Format.printf "Id?\n"
let _ = assert (infer empty_ctx id = Tarrow (Tunit, Tunit))

let ff =
  Vfun
    ( "a",
      Tunit,
      Vfun
        ("f", Tarrow (Tunit, Tproduct (Tunit, Tunit)), Vapp (Vvar "f", Vvar "a"))
    )

let () = Format.printf "ff?\n"

let _ =
  assert (
    infer empty_ctx ff
    = Tarrow
        ( Tunit,
          Tarrow
            (Tarrow (Tunit, Tproduct (Tunit, Tunit)), Tproduct (Tunit, Tunit))
        ))

let app = Vapp (Vapp (ff, Vunit), Vfun ("f", Tunit, Vproduct (Vunit, Vunit)))
let () = Format.printf "app?\n"
let _ = assert (infer empty_ctx app = Tproduct (Tunit, Tunit))

let fst = Vfun ("x", Tproduct (Tunit, Tunit), Vvar "x")
let product = Vapp (fst, Vproduct (Vunit, Vunit))
let () = Format.printf "product?\n"
let _ = assert (infer empty_ctx product = Tproduct (Tunit, Tunit))

let () = Format.printf "Success!\n"
