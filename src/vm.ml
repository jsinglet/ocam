open Format
open Core.Std

type instruction =
    STOP
  | LOAD of value 
  | PUSH of value 
  | DUPL | SWAP | ROT3   | IROT3
  | FST  | SND  | SETFST | SETSND
  | CONS | SPLIT
  | ADD  | SUB  | MULT   | EQUAL | DIV 
  | B_LT   | B_GT
  | CALL | RETURN
  | BRANCH of value * value 
and 
  value =
    ConstInt of int
  | ConstBool of bool
  | ConstString of string
  | Pair of value ref * value ref
  | Adr of instruction list
  | Nil

let pretty = sprintf 

let rec value_to_string v = match v with
  | ConstInt i -> pretty "ConstInt %d" i
  | ConstBool b -> pretty "ConstBool %b" b
  | ConstString s -> pretty "ConstString %s" s
  | Pair (v1,v2)  -> pretty "Pair (%s,%s)" (value_to_string !v1) (value_to_string !v2)
  | Adr l -> let ixns = List.map ~f:(fun x -> to_string x) l in
    let seperated = List.intersperse ixns ~sep:", " in
      "["  ^ (List.fold seperated ~init:"" ~f:(fun acc x -> acc ^ x)) ^ "]"
  | Nil   -> "Nil"
and to_string i = match i with
  | STOP -> "STOP"
  | LOAD v -> pretty "LOAD %s" (value_to_string v)
  | PUSH v -> pretty "PUSH %s" (value_to_string v)
  | DUPL   -> "DUPL"
  | SWAP   -> "SWAP"
  | ROT3   -> "ROT3"
  | IROT3  -> "IROT3"
  | FST    -> "FST"
  | SND    -> "SND"
  | SETFST -> "SETFST"
  | SETSND -> "SETSND"
  | CONS   -> "CONS"
  | SPLIT  -> "SPLIT"
  | ADD    -> "ADD"
  | SUB    -> "SUB"
  | MULT   -> "MULT"
  | EQUAL  -> "EQUAL"
  | DIV    -> "DIV"
  | B_LT   -> "LT"
  | B_GT   -> "GT"
  | CALL   -> "CALL"
  | RETURN -> "RETURN"
  | BRANCH (v1,v2) -> pretty "BRANCH (%s,%s)" (value_to_string v1) (value_to_string v2)
