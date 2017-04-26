open Core.Std
open Lang
open Lexer
     
let usage = "Usage: cam [ -lex | -parse | -vm | -run | -compile ] FILE
-lex:    	Lex the source of the program contained in FILE.
-parse:		Parse the source of program contained in FILE (implies -lex).
-vm:		Execute the VM code contained in FILE.o
-run:           Lex, Parse, and Execute the code contained in FILE.
-compile:       Compile FILE to byte code. Outputs FILE.o.
"
 
let spec =
  let open Command.Spec in
  empty 
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"CAM: The Categorical Abstract Machine"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> print_endline filename)

let add a b = a + b;;

    

let () = 
  Command.run ~version:"1.0" ~build_info:"RWO" command




