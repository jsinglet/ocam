open Core.Std
open Lexer
open Token
open Utils
open Format
 
let getLex text = Lexer.to_string (getTokens text)
let lex f = let text = load_file f in print_endline (getLex text)

let doIt = Token.TokenAdd = Token.TokenAdd

let spec =
  let open Command.Spec in
  empty 
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"CAM: The Categorical Abstract Machine (Lexer)"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> lex filename)

let () = 
  Command.run ~version:"1.0" command
