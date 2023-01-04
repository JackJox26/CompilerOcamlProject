open Ast

%token <string> ID
%token <int> CSTE
%token AFFECT
%token PARENT_G PARENT_D POINTVIRGULE DEUXPOINTS POINT
%token IF THEN ELSE 
%token ACCOLADE_G ACCOLADE_D
%token IS VAR EXTENDS
%token AUTO OBJECT DEF NEW
%token PLUS MOINS MUL DIV UMOINS
%token PG PGE PP PPE EGAL NEGAL (*(OPERATEUR)*)
%token CONCAT
%token EOF

%start<Ast.progType> prog
%%
(* verifie si l'expression renvoie pas  *)
let surcharge e lmethods =
  let rec sur_aux e lmethods classTemp res=
    match e with 
      Object x ->
        match x with 
      |Cste v -> ()
      | Plus(g, d) | Minus (g, d) | Times (g, d) | Div (g, d) ->
        sur_aux g lmethods classTemp res;
        sur_aux g lmethods classTemp res;
        else lmethods = 
  in r

type grid = int list list
let methodClass e =
    let