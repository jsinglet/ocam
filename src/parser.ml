open Core.Std
open Lexer
open Token
open Lang 
open Printf 

let follow = [
  Token.TokenOpenPair; 
  Token.TokenFst; 
  Token.TokenSnd; 
  Token.TokenFun; 
  Token.TokenLet; 
  Token.TokenLetRec; 
  ]

let emiterror e w = eprintf e w; eprintf "\n"

let rec recover tokens = match(tokens) with
  | (x::rest) when List.mem follow x-> rest 
  | (x::rest) -> recover rest 
  | [] -> eprintf "Attempted to recover, but ran out of tokens before finding a valid token.\n"; exit 1


let skip tokens t = match (tokens) with
  | (x::rest) when t = x -> rest
  | (x::rest) ->  eprintf "Parser Error: Expected: %s but found: %s\n"  
                          (Token.to_string t)
                          (Token.to_string x); 
                  recover rest
  | [] -> eprintf "Parser Error: Tried to skip, but ran out of tokens\n"; exit 1


(* ;*)

let rec term tokens = match tokens with
  | (Token.TokenBooleanLiteral b::xs) -> (EXP_BOOL b, xs)
  | (Token.TokenIntegerLiteral i::xs) -> (EXP_INT i, xs)
  | (Token.TokenIdent s::xs)          -> (EXP_VAR s, xs)
  | (Token.TokenOpenParen::xs)        -> let (e, rest) = expression xs in (e, skip rest Token.TokenCloseParen)
  | _ -> failwith ("Parser Error: Unknown Term.\n")

and

  expression (tokens : Token.token list) = match tokens with
  | (Token.TokenOpenPair::xs) ->
    let (p1, rest) = expression xs in
    let rest1 = skip rest Token.TokenComma in
    let (p2, rest2) = expression rest1 in
    let rest3 = skip rest2 Token.TokenClosePair in
    (EXP_PAIR (p1, p2), rest3)
  | (Token.TokenFst::xs) -> let (e1, rest) = expression xs in (EXP_UNOP (M_FST, e1), rest)
  | (Token.TokenSnd::xs) -> let (e1, rest) = expression xs in (EXP_UNOP (M_SND, e1), rest)
  | (Token.TokenIf::xs)  ->
    let (e1, rest) = expression xs in
    let (e2, rest1) = expression (skip rest Token.TokenThen) in
    let (e3, rest2) = expression (skip rest1 Token.TokenElse) in (EXP_IF (e1, e2, e3), rest2)
  | (Token.TokenFun::xs) ->
    (
      match (xs) with
      | (Token.TokenIdent i::xs) -> let rest = (skip xs Token.TokenArrow) in
        let (e1, rest1) = expression rest in
        (EXP_FUN (i, e1), rest1)
      | _                  ->
        eprintf "Expected a functional parameter (ident) following lambda but found: %s\n" 
                  (Token.to_string (List.hd_exn xs)); expression (recover xs))
  | (Token.TokenLet::xs) ->
    (
      match (xs) with
      | (Token.TokenIdent i::xs) -> let rest = (skip xs Token.TokenAssign) in
        let (e1, rest1) = expression rest in
        let rest2 = skip rest1 Token.TokenIn in
        let (e2, rest3) = expression rest2 in
        (EXP_LET (i, e1, e2), rest3)
      | _      ->
        eprintf "Parse Error: Expected Ident following let but found: %s\n" 
                  (Token.to_string (List.hd_exn xs)); expression (recover xs))
  | (Token.TokenLetRec::xs) ->
    (
      match (xs) with
      | (Token.TokenIdent i::xs) -> let rest = (skip xs Token.TokenAssign) in
        let (e1, rest1) = expression rest in
        let rest2 = skip rest1 Token.TokenIn in
        let (e2, rest3) = expression rest2 in
        (EXP_LETREC (i, e1, e2), rest3)
      |_      -> eprintf "Expression Error: Expected Ident following let rec but found: %s\n" (Token.to_string (List.hd_exn xs)); expression (recover xs) )


  | xs -> 
    try
      let (e1, rest) = term xs in
      (match rest with
      | [] -> (e1, rest)
      | (op::rest1) when isOp op ->  (* binary expression *)
        let operator = getOp op in
        let (e2, rest2) = expression rest1 in
        (EXP_BINOP (operator, e1, e2), rest2)
      | (t::rest1) when (isStop t) ->
        (e1, rest)
      | _ -> let (e2, rest1) = expression rest in
        (EXP_APP (e1, e2), rest1))
      with
        _ -> eprintf "Term Error: Expected TERM but found: %s\n" (Token.to_string (List.hd_exn xs)); expression (recover xs) 


let parse tokens = fst (expression tokens)

let lexparse s = Lang.to_string (fst (expression (getTokens s)))