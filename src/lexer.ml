open Core.Std
open String

module Token = struct
  type token =
      TokenBooleanLiteral of bool
    | TokenIntegerLiteral of int
    | TokenStringLiteral of string 
    | TokenEquals          (* i.e., ==  *)
    | TokenLt
    | TokenGt
    | TokenAssign          (*  -- i.e., = *)
    | TokenAdd
    | TokenMinus
    | TokenMultiply
    | TokenDivide
    | TokenOpenParen
    | TokenCloseParen
    | TokenOpenPair
    | TokenClosePair
    | TokenIdent of string 
    | TokenLet
    | TokenLetRec
    | TokenIn
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenFun
    | TokenArrow
    | TokenComma
    | TokenFst
    | TokenSnd

  let to_string t = match t with
    | TokenBooleanLiteral b   -> "TokenBooleanLiteral " ^ Bool.to_string b
    | TokenIntegerLiteral i  -> "TokenIntegerLiteral " ^ Int.to_string i
    | TokenStringLiteral s   -> "TokenStringLiteral " ^ s
    | TokenEquals            -> "TokenEquals"
    | TokenLt                -> "TokenLt"
    | TokenGt                -> "TokenGt"
    | TokenAssign            -> "TokenAssign"
    | TokenAdd               -> "TokenAdd"
    | TokenMinus             -> "TokenMinus"
    | TokenMultiply          -> "TokenMultiply"
    | TokenDivide            -> "TokenDivide"
    | TokenOpenParen         -> "TokenOpenParen"
    | TokenCloseParen        -> "TokenCloseParen"
    | TokenOpenPair          -> "TokenOpenPair"
    | TokenClosePair         -> "TokenClosePair"
    | TokenIdent s           -> "TokenIdent \"" ^ s ^ "\""
    | TokenLet               -> "TokenLet"
    | TokenLetRec            -> "TokenLetRec"
    | TokenIn                -> "TokenIn"
    | TokenIf                -> "TokenIf"
    | TokenThen              -> "TokenThen"
    | TokenElse              -> "TokenElse"
    | TokenFun               -> "TokenFun"
    | TokenArrow             -> "TokenArrow"
    | TokenComma             -> "TokenComma"
    | TokenFst               -> "TokenFst"
    | TokenSnd               -> "TokenSnd"

  let keywords = [
    "let" , TokenLet;
    "rec" , TokenLetRec;
    "in"  , TokenIn;
    "if"  , TokenIf;
    "then", TokenThen;
    "else", TokenElse;
    "fun" , TokenFun;
    "snd" , TokenSnd;
    "fst" , TokenFst
  ]

  let multis = [
    "==", TokenEquals;
    "->", TokenArrow;
  ]

  let specials = [
    "<", TokenLt;
    ">", TokenGt;
    "=", TokenAssign; 
    "+", TokenAdd;
    "-", TokenMinus;
    "(", TokenOpenParen;
    ")", TokenCloseParen;
    "*", TokenMultiply;
    "/", TokenDivide;
    ",", TokenComma;
    "[", TokenOpenPair;
    "]", TokenClosePair
  ]
  let compositeKeywords = [
    (
      (TokenLet,TokenLetRec) (* the adjacent keywords *)
    , TokenLetRec            (* what to replace it with *)
    )
  ]

  let stringForToken tok = let (str,_) = List.hd_exn (List.filter ~f:(function (lexeme,t) -> phys_equal t tok) (List.append specials multis)) in str

  let multiSpecials = List.map ~f:stringForToken [TokenAssign; TokenGt]

  let rec convertCompositeKeywords xs =
    match xs with
    | (x1::x2::rest) ->
      let m = List.Assoc.find compositeKeywords (x1,x2) in
      (match m with
       | Some replacement -> replacement :: (convertCompositeKeywords rest)
       | None -> x1 :: (convertCompositeKeywords (x2::rest))
      )
    | rest -> rest

  (* Functions for Determining Character Classes *)
  let isDigit c = contains "0123456789" c

  let isIgnore c = contains " \t\n\r" c

  let isAlpha c = let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    contains (alphabet ^ (lowercase alphabet)) c

  let isKeyword s = match List.Assoc.find keywords s with
    | Some k -> true
    | _      -> false

  let isSpecial c = match List.Assoc.find specials (Char.to_string c) with
    | Some s -> true
    | _      -> false

  let isMultiSpecial c = List.mem multiSpecials (Char.to_string c)

  let isBoolean s = s = "True" || s = "False"

  let isEquals s  = (stringForToken TokenEquals) = s

  let isArrow s   = (stringForToken TokenArrow)  = s

  let isNumber s  = let l = (filter ~f:(fun x-> not (isDigit x)) s) in Int.equal (String.length l) 0

  (* Functions for converting from strings to Tokens *)
  let singleSpecialToToken c = List.Assoc.find_exn specials c

  let keywordToToken s       = List.Assoc.find_exn keywords s

end

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




