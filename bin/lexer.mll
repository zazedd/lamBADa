{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { MULT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "let" { LET }
  | "=" { EQUALS }
  | "==" { EQ }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "->" { ARROW }
  | id { ID ( Lexing.lexeme lexbuf ) }
  | int { INT ( Lexing.lexeme lexbuf |> int_of_string ) }
  | eof { EOF }

