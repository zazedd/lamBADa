open Eval
open Types

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec interp ctx t_ctx =
  try
    print_string "lamBADa >> ";
    let line = read_line () |> String.trim in
    let ast, typ, res_t_ctx = line |> parse |> Types.typechecker t_ctx in
    let res, res_ctx = ast |> eval ctx in
    if is_value res then
      "- : " ^ string_of_typ typ ^ " = " ^ string_of_val res |> print_endline
    else "- : " ^ string_of_typ typ ^ " = <fun>" |> print_endline;
    interp res_ctx res_t_ctx
  with End_of_file -> exit 0

let () = interp Ctx.empty []
