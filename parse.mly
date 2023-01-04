%{
open Ast
%}
%token <string> ID
%token <int> CSTE
%token <Ast.opType> OPERATEUR
%token PLUS MOINS MUL DIV UMOINS
%token PARENT_G PARENT_D
%tpken ACCOLADE_G ACCOLADE_D
%token POINTVIRGULE DEUXPOINTS POINT
%token CONCAT
%token AFFECT
%token IF THEN ELSE
%token IS VAR
%token AUTO DEF NEW RETURN
%token <Ast.objetType> OBJECT
%token EOF


(* l'axiome est aussi le nom de la fonction a appeler pour faire l'analyse syntaxique *)
%start<Ast.progType> prog
%%
prog:
| lObjets b=bloc EOF      {}

lObjets:
|                       {}

bloc:
| ACCOLADE_G o=optLInstruc ACCOLADE_D             {}
| ACCOLADE_G lDecLVal IS lInstruc ACCOLADE_D    {}

optLInstruc:
|                       {}
| optLInstruc           {}

lInstruc:
| instruc               {}
| instruc lInstruc      {}

instruc:
| bloc                  {}
| RETURN POINTVIRGULE   {}