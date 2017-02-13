open Core.Std 
open Lang
open Vm

let compile_unop e = match (e) with
  | M_FST -> FST
  | M_SND -> SND

let compile_binop e = match (e) with
  | M_ADD  -> ADD
  | M_SUB  -> SUB
  | M_MULT -> MULT
  | M_EQ   -> EQUAL
  | M_LT   -> B_LT
  | M_GT   -> B_GT 
  | M_DIV  -> DIV  

let rec compile_var env v = match env with
  | [] -> failwith ("Compile Error: Unknown Variable" ^ v)
  | (x::envs) -> if x = v then [FST] else [SND] @ (compile_var envs v)

let init_stack = [Nil]

let rec comp env e = match e with
  (* constants *)
  | (EXP_INT n)  -> [LOAD (ConstInt n) ]
  | (EXP_BOOL b) -> [LOAD (ConstBool b)]
  | (EXP_STRING s) -> [LOAD (ConstString s)]
  (* pairs, unary, and expressions *)
  | (EXP_PAIR (e1, e2)) -> [DUPL] @ (comp env e2) @ [SWAP] @ (comp env e1) @ [CONS]
  | (EXP_UNOP (op, e)) -> (comp env e)@[compile_unop op]
  | (EXP_BINOP (op, e1, e2)) -> [DUPL] @ (comp env e2) @ [SWAP] @ (comp env e1) @ [compile_binop op]
  | (EXP_VAR v) -> compile_var env v
  (* control structures *)
  | (EXP_IF (e1, e2, e3)) -> [DUPL]
                             @ (comp env e1)
                             @ [BRANCH (Adr(comp env e2 @ [RETURN]), Adr(comp env e3 @ [RETURN])); CALL]
   (* functions and application *)
  | (EXP_FUN (x, e)) -> [PUSH (Adr(comp (x::env) e @ [RETURN])); SWAP; CONS]

  | (EXP_APP (e1, e2)) -> [DUPL] @ (comp env e2) @ [SWAP] @ (comp env e1)
                          @ [SPLIT; IROT3; CONS; SWAP; CALL]
  | (EXP_LET (x, e1, e2)) -> [DUPL] @ (comp env e1) @ [CONS] @ (comp (x::env) e2)
  | (EXP_LETREC (f, ((EXP_FUN _) as e1), e2)) -> [PUSH Nil] @ (comp (f::env) e1)
                                              @ [DUPL; ROT3; CONS; SETFST; FST]
                                              @ (comp (f::env) e2)

  | (EXP_LETREC (f, e1, e2)) -> [DUPL; PUSH Nil; DUPL; CONS; DUPL; ROT3; CONS]
                                @ (comp (f::env) e1)
                                @ [DUPL; ROT3; FST; SETFST; SWAP; SND; SETSND; CONS]
                                @ (comp (f::env) e2)

 
let compile e = (comp [] e) @ [STOP]

let evalProgram program = exec (program) [] (init_stack)
let evalTree tree = exec (compile tree) [] (init_stack)
let eval s = let tree = Parser.parse (Lexer.getTokens s) in
  evalTree tree

let rec to_string l = let ixns = List.map ~f:(fun x -> Vm.to_string x) l in
  let seperated = List.intersperse ixns ~sep:", " in
  "["  ^ (List.fold seperated ~init:"" ~f:(fun acc x -> acc ^ x)) ^ "]"
