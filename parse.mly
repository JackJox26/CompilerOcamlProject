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
| l=optLInstruc             { l }

lInstruc:
| i= instruc                { [i] }
| i= instruc l= lInstruc    { i::l }

instruc:
(*| e= expr POINTVIRGULE                                  { e }*)
| b= bloc                                               { Bloc(b) }
| RETURN POINTVIRGULE                                   { Return }
(*| IF e= expr THEN i1= instruc ELSE i2= instruc        { IfThenElse(e,i1,i2) }

expr:
| s= ID                     { Id(s) }
| v= CSTE                   { Cste(v) }
| e1= expr PLUS e2= expr    { Plus(e1,e2) }
| e1= expr MOINS e2= expr   { Moins(e1,e2) }
| e1= expr MUL e2= expr     { Mult(e1,e2) }
| e1= expr DIV e2= expr     { Div(e1,e2) }
| PLUS e= expr              { PlusU(e) }
| MOINS e= expr             { MoinsU(e) }
*)