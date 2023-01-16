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
(*%right ELSE*)
%nonassoc OPERATEUR
%left PLUS MOINS CONCAT
%left MUL DIV
%left UNITAIRE
(*%right PARENT_D (*Type cast*)*)
%left POINT (*Element selection by reference*)

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
| s1=ID POINT s2=ID                                     { CibleMembre(AutoRef(s1,s2)) }
| PARENT_G s1=ID PARENT_D POINT s2=ID                   { CibleMembre(AutoRef(s1,s2)) }
| PARENT_G n=NOMCLASSE s1=ID PARENT_D POINT s2=ID       { CibleMembre(MembreMasque(n,s1,s2)) }

expr:
  s= ID                                                 { Id(s) }
| v= CSTE                                               { Cste(v) }
| s= STR                                                { Str(s) }
| PARENT_G e=expr PARENT_D                              { e }
| PARENT_G n=NOMCLASSE e=expr PARENT_D                  { Cast(n,e) }
| s1=ID POINT s2=ID                                     { Membre(AutoRef(s1,s2)) }
| PARENT_G s1=ID PARENT_D POINT s2=ID                   { Membre(AutoRef(s1,s2)) }
| PARENT_G n=NOMCLASSE s1=ID PARENT_D POINT s2=ID       { Membre(MembreMasque(n,s1,s2)) }
| NEW n=NOMCLASSE PARENT_G l=optLParam PARENT_D         { Instance(n,l) }
| e=expr POINT s=ID PARENT_G l=optLParam PARENT_D       { Methode(MethodeExpr(e,s,l)) }
| n=NOMCLASSE POINT s=ID PARENT_G l=optLParam PARENT_D  { Methode(MethodeObjetIsole(n,s,l)) }
| e1= expr PLUS e2= expr                                { Plus(e1,e2) }
| e1= expr MOINS e2= expr                               { Moins(e1,e2) }
| e1= expr MUL e2= expr                                 { Mult(e1,e2) }
| e1= expr DIV e2= expr                                 { Div(e1,e2) }
| e1= expr CONCAT e2= expr                              { Concat(e1,e2) }
| PLUS e= expr %prec UNITAIRE                           { e }
| MOINS e= expr %prec UNITAIRE                          { MoinsU(e) }
| e1=expr o=OPERATEUR e2=expr                           { Comp(e1,o,e2) }

optLExpr:
                            { [] }
| l=lExpr                   { l }

lExpr:
  e=expr                    { [e] }
| e=expr VIRGULE l=lExpr    { e::l }