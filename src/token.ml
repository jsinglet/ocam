open Core.Std
open String

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
  | TokenSkip

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
  | TokenSkip              -> "TokenSkip"

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


