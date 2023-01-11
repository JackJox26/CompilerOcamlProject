
(* The type of tokens. *)

type token = 
  | VIRGULE
  | VAR
  | UMOINS
  | THEN
  | STR of (string)
  | RETURN
  | POINTVIRGULE
  | POINT
  | PLUS
  | PARENT_G
  | PARENT_D
  | OPERATEUR of (Ast.opType)
  | OBJECT
  | NOMCLASSE of (string)
  | NEW
  | MUL
  | MOINS
  | IS
  | IF
  | ID of (string)
  | EXTENDS
  | EOF
  | ELSE
  | DIV
  | DEUXPOINTS
  | DEF
  | CSTE of (int)
  | CONCAT
  | CLASS
  | AUTO
  | AFFECT
  | ACCOLADE_G
  | ACCOLADE_D

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.progType)
