open Core.Std
open Lexer
open Token
open Lang 

let skip tokens t = match (tokens) with
  | (x::rest) when t = x -> rest
  | (x::rest) -> failwith ("Parser Error: Expected: " ^ (Token.to_string t)
                                                         ^ " but found: " ^ (Token.to_string x))
  | [] -> failwith "Parser Error: Tried to skip, but ran out of tokens"

let rec term tokens = match tokens with
  | (Token.TokenBooleanLiteral b::xs) -> (EXP_BOOL b, xs)
  | (Token.TokenIntegerLiteral i::xs) -> (EXP_INT i, xs)
  | (Token.TokenIdent s::xs)          -> (EXP_VAR s, xs)
  | (Token.TokenOpenParen::xs)        -> let (e, rest) = expression xs in (e, skip rest Token.TokenCloseParen)
  | _ -> failwith ("Parser Error: Unknown Term.")

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
        failwith ("Expected a functional parameter (ident) following lambda but found: " ^
                  (Token.to_string (List.hd_exn xs))))
  | (Token.TokenLet::xs) ->
    (
      match (xs) with
      | (Token.TokenIdent i::xs) -> let rest = (skip xs Token.TokenAssign) in
        let (e1, rest1) = expression rest in
        let rest2 = skip rest1 Token.TokenIn in
        let (e2, rest3) = expression rest2 in
        (EXP_LET (i, e1, e2), rest3)
      | _      ->
        failwith ("Parse Error: Expected Ident following let but found: " ^
                  Token.to_string (List.hd_exn xs)))
  | (Token.TokenLetRec::xs) ->
    (
      match (xs) with
      | (Token.TokenIdent i::xs) -> let rest = (skip xs Token.TokenAssign) in
        let (e1, rest1) = expression rest in
        let rest2 = skip rest1 Token.TokenIn in
        let (e2, rest3) = expression rest2 in
        (EXP_LETREC (i, e1, e2), rest3)
      |_      -> failwith ("Expression Error: Expected Ident following letrec but found: " ^
                           (Token.to_string (List.hd_exn xs))))


  | xs -> let (e1, rest) = term xs in
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



let parse tokens = fst (expression tokens)
