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
  | Pair of (sim_ref) * (sim_ref)
  | Adr of instruction list
  | Nil
and
  sim_ref  =
  Ref of (int * (value list -> value))

let pretty = sprintf 

let rec value_to_string ?(mem=[]) v = match v with
  | ConstInt i -> pretty "%d" i
  | ConstBool b -> pretty "%b" b
  | ConstString s -> pretty "%s" s
  | Pair ((Ref (i1,v1)),(Ref (i2,v2)))  -> pretty "#%d" (i1) 
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

let formatBlock name vals = let parts = List.map ~f:(fun x -> value_to_string x) vals in
  let seperated = List.intersperse parts ~sep:":" in
     sprintf "%s %s" name ("["  ^ (List.fold seperated ~init:"" ~f:(fun acc x -> acc ^ x)) ^ "]")

let printVMState code mem values = match (code) with
  | (BRANCH (v1,v2)) -> (printf "BRANCH\n\tIF   =%s\n\tELSE =%s\n%-30s %-10s %-10s\n" (value_to_string v1) (value_to_string v2) "BRANCH CONTROL INFO:" (formatBlock "MEM" mem) (formatBlock "STACK" values))
  | _ -> printf "%-30s %-10s %-10s\n" (to_string code) (formatBlock "MEM" mem) (formatBlock "STACK" values)

let makePair mem v1 v2 =
  let access = fun n m -> List.nth_exn m n in 
  let mr = (List.length mem) in
  let p = Pair ((Ref (mr, (access mr))), (Ref ((mr+1), (access (mr+1))))) in
  (p, mem @ [v1;v2])

let rec replaceLocation n v mem = match mem with
  | (x::xs) when n = 0 -> (v::xs)
  | (x::xs) -> x :: replaceLocation (n-1) v xs
  | _ -> failwith "Memory Access Error."

(* implementation of the VM *)
let rec exec ixns m s = printVMState (List.hd_exn ixns) m s;
  match ixns,s with
  | [STOP], [v]                          -> v
  | ((LOAD v)::code), (_::stack)       -> exec code m (v::stack)
  | ((PUSH v)::code), stack          -> exec code m (v::stack)
  | (DUPL::code), (v::stack)         -> exec code m (v::v::stack)
  | (SWAP::code), (v::v'::stack)       -> exec code m (v'::v::stack)
  | (ROT3::code), (v1::v2::v3::stack)   -> exec code m (v2::v3::v1::stack)
  | (IROT3::code), (v1::v2::v3::stack)  -> exec code m (v3::v1::v2::stack)
  | (ADD::code), ((ConstInt a)::(ConstInt b)::stack) -> exec code m ((ConstInt (a + b))::stack)
  | (SUB::code), ((ConstInt a)::(ConstInt b)::stack) -> exec code m ((ConstInt (a - b))::stack)
  | (MULT::code), ((ConstInt a)::(ConstInt b)::stack) -> exec code m ((ConstInt (a * b))::stack)
  | (EQUAL::code), (a::b::stack) -> exec code m ((ConstBool (a = b))::stack)
  | (B_LT::code), (a::b::stack) -> exec code m ((ConstBool (a < b))::stack)
  | (B_GT::code), (a::b::stack) -> exec code m ((ConstBool (a > b))::stack)
  | (CALL::code), ((Adr code')::v::stack) -> exec code' m (v::(Adr code::stack))
  | (RETURN::_), (v::(Adr code')::stack) ->  exec code' m (v::stack)
  | ((BRANCH (adr1, adr2))::code), ((ConstBool b)::stack) -> if b then
                                                            exec code m (adr1::stack)
                                                       else
                                                           exec code m (adr2::stack)
  | (FST::code), ((Pair ((Ref (_,v1)) ,_))::stack) -> exec code m ((v1 m)::stack)
  | (SND::code), ((Pair (_, (Ref (_, v2))))::stack) -> exec code m ((v2 m)::stack)
  | (CONS::code), (v1::v2::stack) ->  let (p,mem') = makePair m v1 v2 in exec code mem' (p::stack)
  | (SPLIT::code), ((Pair ((Ref (_, v1)), (Ref (_, v2))))::stack) -> exec code m ((v1 m)::(v2 m)::stack)

  | (SETFST::code), (v::(Pair ((Ref (i1, v1)), (Ref (i2, v2))) )::stack) -> let p = Pair ((Ref (i1, v1)), (Ref (i2, v2))) in
  let mem' = replaceLocation i1 v m in exec code mem' (p::stack)
  | (SETSND::code), (v::(Pair ((Ref (i1, v1)), (Ref (i2, v2))) )::stack) -> let p = Pair ((Ref (i1, v1)), (Ref (i2, v2))) in
  let mem' = replaceLocation i2 v m in exec code mem' (p::stack)
  | (x::code), _ -> failwith ("Invalid Instruction Encountered: " ^ (to_string x))
  | _ -> Nil




