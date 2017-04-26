open Core.Std
open Utils
open Compile  

let lex f = let text = load_file f in Lexer.getTokens text

let parse f = let tokens = lex f in
  let parseTree = Parser.parse tokens in 
  parseTree 

let doCompile f = let parseTree = parse f in
  let program = Compile.compile parseTree in
  print_endline (Compile.to_string program);
  let v = Compile.evalProgram program in
  print_endline (Vm.value_to_string v)
 
let spec =
  let open Command.Spec in
  empty 
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"CAM: The Categorical Abstract Machine (Rungime)"
    ~readme:(fun () -> "The CAM Runtime Compiles and Runs CAM code.")
    spec
    (fun filename () ->  printProgram filename; doCompile filename)

let () = 
  Command.run ~version:"1.0" command

let add a b = a + b
