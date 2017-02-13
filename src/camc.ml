open Core.Std
open Utils
open Compile  

let lex f = let text = load_file f in Lexer.getTokens text

let parse f = let tokens = lex f in
  let parseTree = Parser.parse tokens in 
  parseTree 
  
let compile f = let parseTree = parse f in
  let program = Compile.compile parseTree in
  print_endline (Compile.to_string program)

let spec =
  let open Command.Spec in
  empty 
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"CAM: The Categorical Abstract Machine (Compiler)"
    ~readme:(fun () -> "The CAM compiler generates object code.")
    spec
    (fun filename () -> compile filename)

let () = 
  Command.run ~version:"1.0" command
