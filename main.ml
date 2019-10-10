

module ParseCL = Nice_parser.Make(struct
  type result = Input.toplevel
  type token = Parser.token
  exception ParseError = Parser.Error
  let parse = Parser.commandline
  include Lexer
end)

module ParseFile = Nice_parser.Make(struct
  type result = Input.toplevel list
  type token = Parser.token
  exception ParseError = Parser.Error
  let parse = Parser.file
  include Lexer
end)



(* 
val parse_string : ?⁠pos:Stdlib.Lexing.position -> string -> result
val parse_chan : ?⁠pos:Stdlib.Lexing.position -> Stdlib.in_channel -> result
val parse_file : string -> result
*) 


