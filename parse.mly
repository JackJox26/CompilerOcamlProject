%{
open Ast
%}
%token <string> ID
%token <int> CSTE
%token <Ast.opType> OPERATEUR
%token PLUS MOINS MUL DIV UMOINS
%token PARENT_G PARENT_D
%tpken ACCOLADE_G ACCOLADE_D
%token ACCOLADE_G ACCOLADE_D
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
| l= lObjets b= bloc EOF    { Prog(l,b) }

lObjets:
|                           { [] }

bloc:
| ACCOLADE_G o= optLInstruc ACCOLADE_D                  { BlocLInst(o) }
| ACCOLADE_G ld= lDecLVal IS li= lInstruc ACCOLADE_D    { BlocDecl(ld,li) }

optLInstruc:
| optLInstruc           {}
| l=optLInstruc             { l }

lInstruc:
| i= instruc                { [i] }
| i= instruc l= lInstruc    { i::l }

instruc:
| b= bloc                                               { Bloc(b) }
| RETURN POINTVIRGULE                                   { Return }
(*| IF e= expr THEN i1= instruc ELSE i2= instruc          {}

expr:*)