open Core.Std
open Utils


let lex f = let text = load_file f in Lexer.getTokens text

let parse f = let tokens = lex f in
  let parseTree = Parser.parse tokens in 
  print_endline (Lang.to_string parseTree)
  
 
let spec =
  let open Command.Spec in
  empty 
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"CAM: The Categorical Abstract Machine (Parser)"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () ->  printProgram filename; parse filename)

let () = 
  Command.run ~version:"1.0" command
