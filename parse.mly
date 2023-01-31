%{
open Ast
%}
%token <string> ID NOMCLASSE
%token <int> CSTE
%token <string> STR
%token <Ast.opType> OPERATEUR
%token PLUS MOINS MUL DIV
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

%nonassoc OPERATEUR
%left PLUS MOINS CONCAT
%left MUL DIV
%left UNITAIRE
%nonassoc REDUCEID
%right PARENT_D (*cast*)
%left POINT (*methode*)

(* l'axiome est aussi le nom de la fonction a appeler pour faire l'analyse syntaxique *)
%start<Ast.progType> prog
%%


deType:
  DEUXPOINTS n=NOMCLASSE    { n }


param:
  s=ID t=deType             { (s,t) }

optLParam:
                            { [] }
| l=lParam                  { l }

lParam:
  p=param                   { [p] }
| p=param VIRGULE l=lParam  { p::l }

 

expr:
  s= ID                               %prec REDUCEID    { Id(s) }
| v= CSTE                                               { Cste(v) }
| s= STR                                                { Str(s) }
| PARENT_G e=expr PARENT_D                              { e }
| PARENT_G n=NOMCLASSE e=expr PARENT_D                  { Cast(n,e) }
| e=expr POINT s=ID PARENT_G l=optLExpr PARENT_D        { MethodeExpr(e,s,l) }
| s=ID PARENT_G l=optLExpr PARENT_D                     { MethodeExpr(Id("this"),s,l) }
| n=NOMCLASSE POINT s=ID PARENT_G l=optLExpr PARENT_D   { MethodeClasse(n,s,l) }
| s1=ID POINT s2=ID                                     { Membre(s1,s2) } (*this ou super*)
(*| PARENT_G s1=ID PARENT_D POINT s2=ID                   { Membre(s1,s2) } (*this ou super*)*)
| NEW n=NOMCLASSE PARENT_G l=optLExpr PARENT_D          { Instance(n,l) }
| e1= expr PLUS e2= expr                                { Plus(e1,e2) }
| e1= expr MOINS e2= expr                               { Moins(e1,e2) }
| e1= expr MUL e2= expr                                 { Mult(e1,e2) }
| e1= expr DIV e2= expr                                 { Div(e1,e2) }
| e1= expr op=OPERATEUR e2= expr                        { Comp(e1,op,e2) }
| e1= expr CONCAT e2= expr                              { Concat(e1,e2) }
| PLUS e= expr                        %prec UNITAIRE    { e }
| MOINS e= expr                       %prec UNITAIRE    { MoinsU(e) }

lExpr:
  e=expr                    { [e] }
| e=expr VIRGULE l=lExpr    { e::l }

optLExpr:
                            { [] }
| l=lExpr                   { l }


declVar:
  l=lIdent t=deType POINTVIRGULE                        { (l,t) }

lDeclVar:
  d= declVar                { [d] }
| d= declVar l= lDeclVar    { d::l }


cible:
  s=ID                                                  { ChampCible("this",s) }
| s1=ID POINT s2=ID                                     { ChampCible(s1,s2) }       (*s1 -> this ou super*)
| PARENT_G n=NOMCLASSE s1=ID PARENT_D POINT s2=ID       { ChampCibleCast(n,s1,s2) } (*s1 -> this ou super*)


lIdent:
  s=ID                      { [s] }
| s=ID VIRGULE l=lIdent     { s::l }


instruc:
  e=expr POINTVIRGULE                                   { Expr(e) }
| b=bloc                                                { Bloc(b) }
| RETURN POINTVIRGULE                                   { Return }
| IF e=expr THEN i1=instruc ELSE i2=instruc             { IfThenElse(e,i1,i2) }
| c=cible AFFECT e= expr POINTVIRGULE                   { Affectation(c,e) }

lInstruc:
  i=instruc                 { [i] }
| i=instruc l=lInstruc      { i::l }

optLInstruc:
                            { [] }
| l=lInstruc                { l }


bloc:
  ACCOLADE_G o=optLInstruc ACCOLADE_D                   { ([],o) }
| ACCOLADE_G ld=lDeclVar IS li=lInstruc ACCOLADE_D      { (ld,li) }


champ:
  VAR a=boption(AUTO) p=param POINTVIRGULE              { (a,p) }

lChamp:
                            { [] }
| c=champ l=lChamp          { c::l }


methode:
  DEF o=boption(OVERRIDE) s=ID PARENT_G lp=optLParam PARENT_D t=deType AFFECT e=expr        { { nomMethode=s ; listParamMethode=lp ; isOverrideMethode=o ; typeRetour=Some(t) ; corpsMethode=([],[Expr(e)])} }
| DEF o=boption(OVERRIDE) s=ID PARENT_G lp=optLParam PARENT_D ot=option(deType) IS b=bloc   { { nomMethode=s ; listParamMethode=lp ; isOverrideMethode=o ; typeRetour=ot ; corpsMethode=b} }

lMethode:
                            { [] }
| m=methode l=lMethode      { m::l }
 

corpsObjet:
  IS ACCOLADE_G lc=lChamp lm=lMethode ACCOLADE_D        { (lc,lm) }


objet:
  CLASS n=NOMCLASSE PARENT_G lp=optLParam PARENT_D EXTENDS h=NOMCLASSE PARENT_G le=optLExpr PARENT_D b=option(bloc) c=corpsObjet  { { nomObjet=n ; estClasse=true ; listParamClasse=lp ; oNomHeritage=Some(h) ; listArgsHeritage=le ; oConstructObjet=b ; corpsObjet=c  } }
| CLASS n=NOMCLASSE PARENT_G lp=optLParam PARENT_D b=option(bloc) c=corpsObjet                                                    { { nomObjet=n ; estClasse=true ; listParamClasse=lp ; oNomHeritage=None ; listArgsHeritage=[] ; oConstructObjet=b ; corpsObjet=c  } }
| OBJECT n=NOMCLASSE b=option(bloc) IS c=corpsObjet                                                                               { { nomObjet=n ; estClasse=false ; listParamClasse=[] ; oNomHeritage=None ; listArgsHeritage=[] ; oConstructObjet=b ; corpsObjet=c } }

lObjets:
                            { [] }
| o=objet l=lObjets         { o::l }


prog:
  l=lObjets b=bloc EOF      { (l,b) }





































(*
%{
open Ast
%}
%token <string> ID NOMCLASSE
%token <int> CSTE
%token <string> STR
%token <Ast.opType> OPERATEUR
%token PLUS MOINS MUL DIV
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

%right AFFECT
%nonassoc OPERATEUR
%left PLUS MOINS CONCAT
%left MUL DIV
%left UNITAIRE
%nonassoc REDUCEID
%right PARENT_D (*cast*)
%left POINT (*methode*)

(* l'axiome est aussi le nom de la fonction a appeler pour faire l'analyse syntaxique *)
%start<Ast.progType> prog
%%


prog:
  l=lObjets b=bloc EOF      { (l,b) }

lObjets:
                            { [] }
| o=objet l=lObjets         { o::l }

objet:
  CLASS n=NOMCLASSE PARENT_G l=optLParam PARENT_D  h=option(heritage) b=option(bloc) c=corpsObjet         { { nomObjet=n ; isObjetIsole=false ; listParamClasse=l ; oHeritageClasse=h ; oConstructObjet=b ; corpsObjet=c  } }
| OBJECT n=NOMCLASSE b=option(bloc) c=corpsObjet                                                          { { nomObjet=n ; isObjetIsole=true ; listParamClasse=[] ; oHeritageClasse=None ; oConstructObjet=b ; corpsObjet=c } }

corpsObjet:
  IS ACCOLADE_G lc=lChamp lm=lMethode ACCOLADE_D        { (lc,lm) }

heritage:
  EXTENDS n=NOMCLASSE PARENT_G l=optLExpr PARENT_D            { { nomHeritage=n ; listArgsHeritage=l } }

param:
  s=ID t=deType             { (s,t) }

deType:
  DEUXPOINTS s=NOMCLASSE    { s }

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
  VAR a=boption(AUTO) p=param                           { (a,p) }

methode:
  DEF o=boption(OVERRIDE) s=ID PARENT_G lp=optLParam PARENT_D t=deType AFFECT e=expr      { { nomMethode=s ; listParamMethode=lp ; isOverrideMethode=o ; typeRetour=Some(t) ; corpsMethode=([],[Exp(e)])} }
| DEF o=boption(OVERRIDE) s=ID PARENT_G lp=optLParam PARENT_D ot=option(deType) IS b=bloc  { { nomMethode=s ; listParamMethode=lp ; isOverrideMethode=o ; typeRetour=ot ; corpsMethode=b} }

lMethode:
                            { [] }
| m=methode l=lMethode      { m::l }

bloc:
  ACCOLADE_G o=optLInstruc ACCOLADE_D                                       { ([],o) }
| ACCOLADE_G ld=lDeclVar POINTVIRGULE IS li=lInstruc ACCOLADE_D             { (ld,li) }

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
  l=lIdent t=deType POINTVIRGULE                        { (l,t) }

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
| s1=ID POINT s2=ID                                     { MembreCible(s1,s2) }
| PARENT_G s1=ID PARENT_D POINT s2=ID                   { MembreCible(s1,s2) }
| PARENT_G n=NOMCLASSE s1=ID PARENT_D POINT s2=ID       { MembreCibleCast(n,s1,s2) }

expr:
  s= ID                             %prec REDUCEID      { Id(s) }
| v= CSTE                                               { Cste(v) }
| s= STR                                                { Str(s) }
| PARENT_G e=expr PARENT_D                              { e }
| PARENT_G n=NOMCLASSE e=expr PARENT_D                  { Cast(n,e) }
| s1=ID POINT s2=ID                                     { Membre(s1,s2) }
| PARENT_G s1=ID PARENT_D POINT s2=ID                   { Membre(s1,s2) }
| NEW n=NOMCLASSE PARENT_G l=optLParam PARENT_D         { Instance(n,l) }
| e=expr POINT s=ID PARENT_G l=optLParam PARENT_D       { MethodeExpr(e,s,l) }
| n=NOMCLASSE POINT s=ID PARENT_G l=optLParam PARENT_D  { MethodeLocal(n,s,l) }
| e1= expr PLUS e2= expr                                { Plus(e1,e2) }
| e1= expr MOINS e2= expr                               { Moins(e1,e2) }
| e1= expr MUL e2= expr                                 { Mult(e1,e2) }
| e1= expr DIV e2= expr                                 { Div(e1,e2) }
| e1= expr CONCAT e2= expr                              { Concat(e1,e2) }
| PLUS e= expr                        %prec UNITAIRE    { e }
| MOINS e= expr                       %prec UNITAIRE    { MoinsU(e) }
| e1=expr o=OPERATEUR e2=expr                           { Comp(e1,o,e2) }

optLExpr:
                            { [] }
| l=lExpr                   { l }

lExpr:
  e=expr                    { [e] }
| e=expr VIRGULE l=lExpr    { e::l }
     
*)