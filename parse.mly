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
(* c=classe                  { Classe(c) }
|*) o=objetIsole              { ObjetIsole(o) }

(*classe:
  CLASS n=NOMCLASSE PARENT_G l=optLParam PARENT_D  h=option(heritage) b=option(bloc) c=corpsObjet         { { nomClasse=n ; listParamClasse=l ; oHeritageClasse=h ; oConstructClasse=b ; corpsClasse=c  } }
*)
corpsObjet:
  IS ACCOLADE_G lc=lChamp lm=lMethode ACCOLADE_D    { Corps(lc,lm) }


(*
heritage:
  EXTENDS s=ID PARENT_G l=optLParam PARENT_D            { Heritage() }
*)

objetIsole:
  OBJECT n=NOMCLASSE b=option(bloc) c=corpsObjet        { { nomObjetIsole=n ; oConstructObjetIsole=b ; corpsObjetIsole=c } }

param:
  s=ID t=deType             { Param(s,t) }

deType:
  DEUXPOINTS s=NOMCLASSE    { Type(s) }

optLParam:
                            { [] }
| l=lParam                  { l }

lParam:
  p=param                   { [p] }
| p=param VIRGULE l=lParam  { p::l }

lChamp:
                            { [] }
| c=champ l=lChamp          { c::l }

champ:
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

optLInstruc:
                            { [] }
| l=lInstruc                { l }

lInstruc:
  i=instruc                 { [i] }
| i=instruc l=lInstruc      { i::l }

lDeclVar:
  d= declVar                { [d] }
| d= declVar l= lDeclVar    { d::l }

declVar:
  l=lIdent t=deType         { Decl(l,t) }

lIdent:
  s=ID                      { [s] }
| s=ID VIRGULE l=lIdent     { s::l }

instruc:
  e=expr POINTVIRGULE                                   { Exp(e) }
| b=bloc                                                { Bloc(b) }
| RETURN POINTVIRGULE                                   { Return }
| IF e=expr THEN i1=instruc ELSE i2=instruc             { IfThenElse(e,i1,i2) }
| c=cible AFFECT e= expr                                { Affectation(c,e) }

cible:
  s=ID                                                  { Var(s) }
| m=membre                                              { CibleMembre(m) }

expr:
  s= ID                                                 { Id(s) }
| v= CSTE                                               { Cste(v) }
| s= STR                                                { Str(s) }
| PARENT_G e=expr PARENT_D                              { e }
| PARENT_G n=NOMCLASSE e=expr PARENT_D                  { Cast(n,e) }
| m=membre                                              { Membre(m) }
| NEW n=NOMCLASSE PARENT_G l=optLParam PARENT_D         { Instance(n,l) }
| m=methodeDeMembre                                     { Methode(m) }
| e1= expr PLUS e2= expr                                { Plus(e1,e2) }
| e1= expr MOINS e2= expr                               { Moins(e1,e2) }
| e1= expr MUL e2= expr                                 { Mult(e1,e2) }
| e1= expr DIV e2= expr                                 { Div(e1,e2) }
| e1= expr CONCAT e2= expr                              { Concat(e1,e2) }
| PLUS e= expr %prec UPLUS                              { e }
| MOINS e= expr %prec UMOINS                            { MoinsU(e) }
| e1=expr o=OPERATEUR e2=expr                           { Comp(e1,o,e2) }

optLExpr:
                            { [] }
| l=lExpr                   { l }

lExpr:
  e=expr                    { [e] }
| e=expr VIRGULE l=lExpr    { e::l }

membre:
  s1=ID POINT s2=ID                                     { AutoRef(s1,s2) }
| PARENT_G s1=ID PARENT_D POINT s2=ID                   { AutoRef(s1,s2) }
| PARENT_G n=NOMCLASSE s1=ID PARENT_D POINT s2=ID       { MembreMasque(n,s1,s2) }

methodeDeMembre:
  e=expr POINT s=ID PARENT_G l=optLParam PARENT_D       { MethodeExpr(e,s,l) }
| n=NOMCLASSE POINT s=ID PARENT_G l=optLParam PARENT_D  { MethodeObjetIsole(n,s,l) }
