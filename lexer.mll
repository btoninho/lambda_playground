

{
open Parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)
    
let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | "(*"
    { comment 0 lexbuf; next_token lexbuf }

  (* YOUR TOKENS HERE... *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { PERIOD }
  | "bool" { BOOL }
  | "fun"  { LAMBDA }
  | "if"   { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | ',' { COMMA }
  | ':' { COLON }
  | "->" { ARROW }
  | "=>" { DARROW }
  | "Check" { CHECK }
  | "Load" { LOAD }
  | "Eval" { EVAL }
  | '"' { string (B.create 100) lexbuf }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | ident as atom { ATOM atom }

  (* no match? raise exception *)
  | _ as c { illegal c }


(* allow nested comments, like OCaml *)
and comment nesting = parse
  | "(*"
    { comment (nesting+1) lexbuf }
  | "*)"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
   { comment nesting lexbuf }
      
(* strings *)
and string buf = parse
 | [^'"' '\n' '\\']+  
            { B.add_string buf @@ get lexbuf
            ; string buf lexbuf 
            }
| '\n'      { B.add_string buf @@ get lexbuf
            ; L.new_line lexbuf
            ; string buf lexbuf
            }
| '\\' '"'  { B.add_char buf '"'
            ; string buf lexbuf
            }
| '\\'      { B.add_char buf '\\'
            ; string buf lexbuf
            }
| '"'       { B.contents buf } (* return *)
| eof       { error lexbuf "end of input inside of a string" }
| _         { error lexbuf 
                "found '%s' - don't know how to handle" @@ get lexbuf }
