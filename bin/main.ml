open Eval

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec interp ctx t_ctx =
  try
    print_string "lamBADa >> ";
    let line = read_line () |> String.trim in
    let ast, res_t_ctx = line |> parse |> Types.typechecker t_ctx in
    let res, res_ctx = ast |> eval ctx in
    string_of_val res |> print_endline;
    interp res_ctx res_t_ctx
  with End_of_file -> exit 0

let () = interp Ctx.empty []
