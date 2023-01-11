%{
open Ast
%}
%token <string> ID NOMCLASSE
%token <int> CSTE
%token <string> STR
%token <Ast.opType> OPERATEUR
%token PLUS MOINS MUL DIV UMOINS
%token PARENT_G PARENT_D
%token ACCOLADE_G ACCOLADE_D
%token VIRGULE POINTVIRGULE DEUXPOINTS POINT
%token CONCAT
%token AFFECT
%token IF THEN ELSE
%token IS VAR
%token CLASS EXTENDS
%token OVERRIDE AUTO DEF NEW RETURN OBJECT
%token EOF

%right ELSE
%left PLUS MOINS CONCAT
%left MUL DIV
%left UPLUS UMOINS


(* l'axiome est aussi le nom de la fonction a appeler pour faire l'analyse syntaxique *)
%start<Ast.progType> prog
%%


prog:
  l=lObjets b=bloc EOF      { Prog(l,b) }

lObjets:
                            { [] }
| o=objet l=lObjets         { o::l }

objet:
<<<<<<< HEAD
(*| c=classe                  { Classe(c) }*)
  o=objetIsole              { ObjetIsole(o) }
=======
  c=classe                  { Classe(c) }
| o=objetIsole              { ObjetIsole(o) }
>>>>>>> main

classe:
  CLASS n=NOMCLASSE PARENT_G l=optLParam PARENT_D  h=option(heritage) b=option(bloc) c=corpsObjet         { { nomClasse=n ; listParamClasse=l ; oHeritageClasse=h ; oConstructClasse=b ; corpsClasse=c  } }

corpsObjet:
<<<<<<< HEAD
  IS ACCOLADE_G lc=lChamp (*lm=lMethode*) ACCOLADE_D    { Corps(lc(*,lm*)) }
=======
  IS ACCOLADE_G lc=lChamp lm=lMethode ACCOLADE_D        { Corps(lc,lm) }
>>>>>>> main

heritage:
  EXTENDS n=NOMCLASSE PARENT_G l=optLExpr PARENT_D      { { nomHeritage=n ; listArgsHeritage=l } }

objetIsole:
<<<<<<< HEAD
  OBJECT n=NOMCLASSE b=option(bloc) c=corpsObjet        { { nomObjetIsole=n ; oConstruct=b ; corpsObjetIsole=c } }
=======
  OBJECT n=NOMCLASSE b=option(bloc) c=corpsObjet        { { nomObjetIsole=n ; oConstructObjetIsole=b ; corpsObjetIsole=c } }
>>>>>>> main

param:
  s=ID t=deType             { Param(s,t) }

deType:
  DEUXPOINTS s=NOMCLASSE    { Type(s) }
<<<<<<< HEAD
=======

optLParam:
                            { [] }
| l=lParam                  { l }

lParam:
  p=param                   { [p] }
| p=param VIRGULE l=lParam  { p::l }
>>>>>>> main

lChamp:
                            { [] }
| c=champ l=lChamp          { c::l }

champ:
<<<<<<< HEAD
  VAR a=boption(AUTO) p=param  { Champs(a,p) }

bloc:
  ACCOLADE_G o=optLInstruc ACCOLADE_D                   { BlocLInst(o) }
| ACCOLADE_G ld=lDeclVar IS li=lInstruc ACCOLADE_D      { BlocDecl(ld,li) }
=======
  VAR a=boption(AUTO) p=param                           { Champs(a,p) }

methode:
  DEF o=boption(OVERRIDE) s=ID PARENT_G lp=optLParam PARENT_D c=methodeCorps        { { nomMethode=s ; listParamMethode=lp ; isOverrideMethode=o ; corpsMethode=c} }

methodeCorps:
  t=deType AFFECT e=expr                                { Val(t,e) }
| t=option(deType) IS b=bloc                            { ResultType(t,b) }

lMethode:
                            { [] }
| m=methode l=lMethode      { m::l }

bloc:
  ACCOLADE_G o=optLInstruc ACCOLADE_D                                               { BlocLInst(o) }
| ACCOLADE_G ld=lDeclVar POINTVIRGULE IS li=lInstruc ACCOLADE_D                     { BlocDecl(ld,li) }
>>>>>>> main

optLInstruc:
                            { [] }
| l=lInstruc                { l }

lInstruc:
  i=instruc                 { [i] }
| i=instruc l=lInstruc      { i::l }

lDeclVar:
<<<<<<< HEAD
  d= declVar                { [d] }
| d= declVar l= lDeclVar    { d::l }

declVar:
  l=lIdent t=deType         { Decl(l,t) }
=======
  d= declVar                                            { [d] }
| d= declVar l= lDeclVar                                { d::l }

declVar:
  l=lIdent t=deType POINTVIRGULE                        { Decl(l,t) }
>>>>>>> main

lIdent:
  s=ID                      { [s] }
| s=ID VIRGULE l=lIdent     { s::l }

<<<<<<< HEAD
instruc: 
=======
instruc:
>>>>>>> main
  e=expr POINTVIRGULE                                   { Exp(e) }
| b=bloc                                                { Bloc(b) }
| RETURN POINTVIRGULE                                   { Return }
| IF e=expr THEN i1=instruc ELSE i2=instruc             { IfThenElse(e,i1,i2) }

expr:
  s= ID                     { Id(s) }
| v= CSTE                   { Cste(v) }
| s= STR                    { Str(s) }
| e1= expr PLUS e2= expr    { Plus(e1,e2) }
| e1= expr MOINS e2= expr   { Moins(e1,e2) }
| e1= expr MUL e2= expr     { Mult(e1,e2) }
| e1= expr DIV e2= expr     { Div(e1,e2) }
| e1= expr CONCAT e2= expr  { Concat(e1,e2) }
| PLUS e= expr %prec UPLUS  { e }
| MOINS e= expr %prec UMOINS{ MoinsU(e) }

<<<<<<< HEAD
=======
optLExpr:
                            { [] }
| l=lExpr                   { l }

lExpr:
  e=expr                    { [e] }
| e=expr VIRGULE l=lExpr    { e::l }
>>>>>>> main
