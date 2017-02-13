open Core.Std
open String
open Token
 
let to_string tokens = let string_tokens = List.map ~f:(fun x -> to_string x) tokens in
  let seperated = List.intersperse string_tokens ~sep:", " in
  "["  ^ (List.fold seperated ~init:"" ~f:(fun acc x -> acc ^ x)) ^ "]"

(** Main Lexer Implementation **)

let acceptAlpha a s = match (String.rev a) with
  | a' when isKeyword a' -> (keywordToToken a', s)
  | a' when isBoolean a' -> (TokenBooleanLiteral (Bool.of_string a'), s)
  | a' when isEquals  a' -> (TokenEquals, s)
  | a'                   -> (TokenIdent a', s)

let acceptNumber a s = match (String.rev a) with
  | a' when isNumber a' -> (TokenIntegerLiteral (Int.of_string a'), s)
  | a'                  -> failwith ("Invalid number: " ^ a')

let acceptSpecial a s = match (String.rev a) with
  | a' when isEquals a' -> (TokenEquals, s)
  | a' when isArrow  a' -> (TokenArrow, s)
  | a' -> failwith ("Unexpected Token: " ^ a')

let rec read acc xs f accept = match xs with
  | (x::rest) when not (f x) -> accept acc xs
  | (x::rest) -> read (Char.to_string x ^ acc) rest f accept
  | [] -> failwith "Lexer Error: Attempted to read token but instead found end of stream"

let readSpecial acc xs = read acc xs isSpecial acceptSpecial
let readAlpha acc xs   = read acc xs isAlpha   acceptAlpha
let readNumber acc xs  = read acc xs isDigit   acceptNumber 

let rec tokenize s = match s with
  | (x::xs) when isSpecial x -> let t = singleSpecialToToken (Char.to_string x) in
    (* it's either a special or an arrow *)
    if isMultiSpecial (List.hd_exn xs) then let (t, rest) = readSpecial (Char.to_string x) xs in t :: tokenize rest
    else t :: tokenize xs
  | (x::xs) when isDigit x -> let (t,rest) = readNumber (Char.to_string x) xs in t :: tokenize rest  (* numbers *)
  | (x::xs) when isIgnore x -> tokenize xs                                                           (* ignores *)
  | (x::xs) when isAlpha x -> let (t, rest) = readAlpha (Char.to_string x) xs in t :: tokenize rest  (* keywords, idents, etc *)
  | [] -> []
  | s' -> failwith ("Tokenizer failed on " ^ (List.to_string ~f:(fun x -> Char.to_string x) s'))

(* life is easier if we just have a empty space at the end of the file *)
let getTokens s = tokenize (String.to_list((s ^ " "))) |> convertCompositeKeywords

