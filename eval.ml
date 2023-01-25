open Ast
(*
type = string                                             -> nomClasse
  integer classe avec methode toString
  String classe avec methodes print et println


tabVars :     Hashtbl (string * type)                     -> (nomVar, typeVar)
tabClasses :  Hashtbl (string * (string * tabMethodes))   -> (nomClasse, (heritageClasseParente, tabMethodesMembres))

tabMethodes : Hashtbl (string * type)                     -> (nomMethode, typeRetour)
dont methode CONSTRUCTEUR
*)

(* retourne le type de s *)
let classeGetType n tabClasses =
  try (Hashtbl.find tabClasses n ; Classe(n))
  with Not_found -> raise (VC_Error ("classe non declaree : " ^ n))

let classeDeclare n tabClasses = classeGetType n tabClasses ; ()


(* retourne le type de s *)
let variableGetType s tabVars =
  try (Hashtbl.find tabVars s)
  with Not_found -> VC_Error ("variable non declaree : " ^ s)

let variableDeclare s tabVars = variableGetType s tabVars ; ()


(* retourne le type de retour de la methode *)
let methodeMembreGetType methode typeClasse tabVars tabClasses
  match type_c with
    | Classe(c) ->  let rec mmgt_rec c_rec =
                      try match (Hashtbl.find tabClasses c_rec) with
                        | (h, tab_m) ->
                            try (Hashtbl.find tab_m methode)
                            with Not_found -> mmgt_rec h
                      with Not_found -> (VC_Error ("classe non declaree : " ^ tc))
                    in mmgt_rec c
    | Integer(_) -> raise (VC_Error ("appel de methode membre, type incompatible : Integer"))
    | String(_) -> raise (VC_Error ("appel de methode membre, type incompatible : String"))




(* retourne tabVars actualise *)
let vc_param p tabVars tabClasses=
  match p with
    (s, t) -> Hashtbl.add tabVars s (classeGetType t tabClasses)

(* retourne tabVars actualise *)
let vc_lparam lpram tabVars tabClasses=
  let rec vc_lp lp_rec tabVars_rec =
    match lp_rec with
      | [] -> tabVars_rec
      | p::l -> vc_lp l (vc_param p tabVars_rec tabClasses)
  in vc_lp lpram

(* retourne son type *)
let vc_expr expr tabVars tabClasses =
  let rec vc_e e_rec =
    match e_rec with
      | Id s -> variableGetType s tabVars
      | Cste v -> Integer(v)
      | Str s -> String(s)
      | Cast (n, e) ->
          match (vc_e e) with
          | Classe(c) ->  try match (Hashtbl.find tabClasses c) with
                            | (*TODO*)
                          with Not_found -> (VC_Error ("classe non declaree : " ^ tc))
          | Integer(_) -> raise (VC_Error ("impossible de caster, type incompatible : Integer"))
          | String(_) -> raise (VC_Error ("impossible de caster, type incompatible : String"))
                    classeGetType n tabClasses
      | Membre(s1,s2) ->
          variableDeclare s1 tabVars;
          methodeMembreDeclare s2 (type_expr s1 lVars) tabClasses
      | Instance(n,l) ->
          classeDeclare n tabClasses;
          vc_lparam l tabVars
      | MethodeExpr(e,s,l) ->
          vc_e e;
          vc_e Id(s);
          vc_lparam l tabVars
      | MethodeStatic(n,s,l) ->
          classeDeclare n tabClasses;
          vc_e Id(s);
          vc_lparam l tabVars
      | Plus(e1,e2) | Moins(e1,e2) | Mult(e1,e2) | Div(e1,e2) | Concat(e1,e2) ->
          vc_e e1;
          vc_e e2
      | MoinsU(e) ->
          vc_e e
      | Comp(e1,o,e2) ->
          vc_e e1;
          vc_e e2
      | ne ->
          vc_e ne
  in vc_e e_rec


let vc_instruc instruc tabVars tabClasses =
  let rec vc_i i_rec =
    match i_rec with
        Expr(e) -> vc_expr e tabVars tabClasses
      | Bloc(b) -> vc_bloc b tabVars tabClasses
      | Return -> ()
      | IfThenElse(e, i1, i2) -> vc_expr e tabVars tabClasses; vc_i i1 tabVars tabClasses; vc_i i2 tabVars tabClasses
      | Affectation(c, e) -> vc_cible c tabVars tabClasses; vc_expr e tabVars tabClasses
  in vc_i instruc


let vc_cible cible tabVars tabClasses =
  match cible with
      Var(s) -> variableDeclare s tabVars
    | MembreCible(s1, s2) -> vc_expr Membre(s1, s2) tabVars tabClasses
    | MembreCibleCast(n, s1, s2) -> classeDeclare n tabClasses ; vc_expr Membre(s1, s2) (s1,n)::tabVars tabClasses

  

(*
(* lance les vérifications contextuelles sur la liste de déclarations ainsi
 * que l'expression finale. D'après l'énoncé il s'agit ici de vérifier que
 * les expressions ne référencent que des variables déclarées et qu'une variable
 * n'est déclarée qu'une fois. Pour cela on va simplement construire une
 * liste des variables déjà déclarées. Ici on n'a pas besoin de connaitre la
 * valeur de ces variables, juste leur existence.
 * L'énoncé demande que cette vérification soit faite avant l'exécution et qu'on
 * reporte le fait qu'une variable ne soit pas déclarée indépendamment du fait
 * qu'on ait besoin ou pas de sa valeur à l'exécution.
 *)
let vc ld e_rec =
  (* Construit progressivement grace a List.fold_left la liste des
   * variables deja rencontrées tout en procédant aux vérifications.
   * On peut aussi faire cela avec une fonction récursive si on ne veut pas
   * recourir à fold_left
   *)
  let allVars =
    List.fold_left (* voir la doc de fold_left pour le rôle des 3 arguments *)
      (fun tabVars decl ->
        (* prend en paramètre l'accumulateur, ie. la liste des variables déjà
         * déclarées (initialement [], le 2eme argument de fold_left) et la
         * déclaration à traiter. *)
        let (lhs, rhs) = decl in
      vc rhs tabVars; (* verifier la partie droite de la déclaration *)

        (* vérifier que lhs n'a pas dejà été déclarée *)
        if List.mem lhs tabVars then
          raise (VC_Error ("redeclaration de la variable " ^ lhs));

        (* renvoie une liste avec la nouvelle variable ajoutée à la liste des
         * variables connues. L'ordre des variables dans la liste n'important
         * pas ici, on la met en tête puisque c'est plus pratique
         *)
        lhs::tabVars
      ) (* fin de la fonction qui est le premier argument de fold_left *)
      [] (* valeur initiale de l'accumulateur *)
      ld (* liste a parcourir par fold_left *)
  in
  (* on a recupéré la liste de toutes les variables déclarées, il ne reste
   * plus qu'à vérifier l'expression finale
   *)
vc e_rec allVars


let eval ld e_rec =
  (* evalDecl: prend une liste de declarations et renvoie une liste
   * (variable, valeur) qui associe à chaque variable le résultat de
   * l'evaluation de l'expression en partie droite de la déclaration.
   *
   * ld : la liste des declarations a traiter
   * env : la liste d'association résultant des declarations deja traitees.
   *
   * On aurait pu de nouveau utiliser List.fold_left. On montre ici une
   * version avec une fonction récursive qui parcourt la liste à la main.
   *)
  let rec evalDecl ld env =
    match ld with
      (* On a traité toutes les déclarations => on renvoie l'environnement *)
      [] -> env
    | (* Evalue la partie droite dans l'environnement courant et ajoute le
       * nouveau couple (variable, valeur) à l'environnement transmis pour
       * l'appel suivant.
       *)
      (lhs, rhs) :: ld' ->
       evalDecl ld' ((lhs, evalExpr rhs env) :: env)
  and evalComp condition env =
    match condition with
      Comp(op, g, d) ->
       let vg = evalExpr g env and vd = evalExpr d env in
       begin
         match op with
           Eq ->  vg = vd
         | Neq -> vg != vd
         | Lt ->  vg < vd
         | Le ->  vg <= vd
         | Gt ->  vg > vd
         | Ge ->  vg >= vd
       end
    | _ ->
       (* une comparaison est forcement de la forme Comp(...). Si on trouve
        * autre chose c'est qu'il y a un pb: AST mal construit
        *)
       failwith "unexpected situation in evalComp"
  and evalExpr e_rec env =
    match e_rec with
      Id x ->
       (* L'exception ne peut pas arriver si les vérifications contextuelles
        * sont correctes.
        *)
       begin
         (* begin end nécessaire pour que les autres cas du match ne soient
          * pas considérées comme des noms d'exceptions gérées par le
          * try ... with qui accepte en général plusieurs exceptions
          *)
         try List.assoc x env
         with Not_found -> failwith ("Unexpected situation in evalExpr")
       end
    | Cste v       -> v
    | Plus(g, d)   -> (evalExpr g env) + (evalExpr d env)
    | Minus (g, d) -> (evalExpr g env) - (evalExpr d env)
    | Times (g, d) -> (evalExpr g env) * (evalExpr d env)
    | Div (g, d)   ->
       let vg = (evalExpr g env) and vd = (evalExpr d env) in
       if vd = 0 then
         raise (RUN_Error "division par 0")
       else vg / vd
    | UMinus e_rec     ->  - (evalExpr e_rec env)
    | Ite (si, alors, sinon) ->
       if evalComp si env then (evalExpr alors env)
       else (evalExpr sinon env)
    | Comp _ ->
       (* une expression de comparaison ne peut pas apparaitre ailleurs que
        * dans un IF et elle sera alors traitée par evalComp. Si on la voit
        * ici c'est qu'il y a un probleme.
        *)
       failwith "unexpected situation in evalExpr"
  in let final_env = evalDecl ld [] in (* traite les déclarations *)
     evalExpr e_rec final_env (* traite l'expression finale *)
;;*)