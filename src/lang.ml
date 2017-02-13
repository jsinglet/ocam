open Core.Std
open Lexer
open Format 

type m_unop = M_FST | M_SND

type m_binop = M_ADD | M_SUB | M_MULT | M_EQ | M_LT | M_GT | M_DIV

type exp = EXP_INT of int | EXP_BOOL of bool | EXP_STRING of string | EXP_PAIR of exp * exp
  | EXP_UNOP of m_unop   * exp 
  | EXP_BINOP of m_binop * exp * exp
  | EXP_VAR of string 
  | EXP_IF of exp * exp * exp
  | EXP_FUN of string * exp 
  | EXP_APP of exp    * exp
  | EXP_LET of string * exp * exp
  | EXP_LETREC of string * exp * exp

let terms = [Token.TokenAdd; Token.TokenMinus; Token.TokenEquals]

let factors = [Token.TokenMultiply; Token.TokenDivide; Token.TokenGt; Token.TokenLt;]

let ops = terms @ factors

let stops = [Token.TokenIn; Token.TokenCloseParen; Token.TokenClosePair; Token.TokenThen; Token.TokenElse; Token.TokenComma] 

let isStop t = List.mem stops t
 
let isOp t = List.mem ops t

let getOp t = match t with
  | Token.TokenAdd -> M_ADD
  | Token.TokenMinus -> M_SUB
  | Token.TokenMultiply -> M_MULT 
  | Token.TokenEquals -> M_EQ
  | Token.TokenLt -> M_LT
  | Token.TokenGt -> M_GT
  | Token.TokenDivide -> M_DIV
  | _       -> failwith ("Parser Error: Token.Token is not an operator : " ^ Token.to_string t)

(* some pretty printing functions *)
let pretty = sprintf

let rec to_string e = match (e) with
  | EXP_INT i -> pretty "(EXP_INT %d)" i
  | EXP_BOOL b -> pretty "(EXP_BOOL %b)" b
  | EXP_STRING s -> pretty "(EXP_STRING %s)" s
  | EXP_PAIR (e1, e2) -> pretty "(EXP_PAIR (%s, %s))" (to_string e1) (to_string e2)
  | EXP_UNOP (op,e1)  -> let s = (match (op) with
      | M_FST -> "M_FST"
      | M_SND -> "M_SND"
    )
    in pretty "(EXP_UNOP %s %s)" s (to_string e1)
  | EXP_BINOP (op,e1,e2) -> let s = (match op with
      | M_ADD -> "+"
      | M_SUB -> "-"
      | M_MULT-> "*"
      | M_EQ  -> "=="
      | M_LT  -> "<"
      | M_GT  -> ">"
      | M_DIV -> "/"
    )
    in pretty "(EXP_BINOP (%s  %s %s))" (to_string e1) s  (to_string e2)
  | EXP_VAR v -> pretty "(EXP_VAR \"%s\")" v
  | EXP_IF (e1,e2,e3) -> pretty "(EXP_IF (%s, %s, %s) )" (to_string e1) (to_string e2) (to_string e3)
  | EXP_FUN (i,e1)    -> pretty "(EXP_FUN \"%s\" (%s) )" i (to_string e1)
  | EXP_APP (e1,e2)   -> pretty "(EXP_APP (%s) (%s))" (to_string e1) (to_string e2)
  | EXP_LET (i, e1, e2) -> pretty "(EXP_LET \"%s\" (%s,%s))" i (to_string e1) (to_string e2)
  | EXP_LETREC (i, e1, e2) -> pretty "(EXP_LETREC \"%s\" (%s,%s))" i (to_string e1) (to_string e2)

