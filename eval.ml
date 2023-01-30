open Ast

(*
    (*nomClasse*)
    type typeType = string

    (*nomVar : typeVar*)
    type tabVars = (string, typeType) Hashtbl.t
    (*dont this du type de la classe courante
      dont super du type du parent de la classe courante*)

    (*nomMethode : lParamType, typeRetour*)
    type tabMethodes = (string, string list * typeType) Hashtbl.t
    (*dont methode 0_construct qui correspond  aux constructeurs de la classe*)

    (*nomClasse : (heritageClasseParente, tabMethodesMembres, tabChamps)*)
    type tabClasses = (string, (typeType option * tabMethodes * tabVars)) Hashtbl.t
*)

exception Not_here



(* retourne le type de la classe (i.e. elle-meme) *)
let classeGetType nomClasse tabClasses =
  try (Hashtbl.find tabClasses nomClasse ; nomClasse)
  with Not_found -> raise (VC_Error ("classe non declaree : " ^ nomClasse))


(* retourne le type de la variable *)
let variableGetType nomVar tabVars =
  try (Hashtbl.find tabVars nomVar)
  with Not_found -> raise (VC_Error ("variable non declaree : " ^ nomVar))


(* retourne le type de retour de la methode *)
let methodeMembreGetType typeClasse nomMethode paramMethode tabClasses =
  let rec matchParam paramAttendus paramFournits =
    match paramFournits with
      | [] -> if(paramAttendus=[]) then true else false
      | f::rf ->
          match paramAttendus with
            | [] -> false
            | a::ra ->
                if(f=a) then
                  matchParam ra rf
                else
                  try match (Hashtbl.find tabClasses f) with
                      | (hopt_paramFournit, _, _) -> 
                          match hopt_paramFournit with
                            | Some(h_paramFournit) -> matchParam paramAttendus (h_paramFournit::rf)
                            | None -> false
                  with Not_found -> false
  in
    let rec matchSignatureGetType lposibilites =
      match lposibilites with
      | [] -> raise (Not_here)
      | posib::r ->
          match posib with (parmPosib, resPosib) ->
            if (matchParam parmPosib paramMethode) then
              resPosib
            else
              matchSignatureGetType r
    in
      let rec mmgt_rec c_rec =
        try match (Hashtbl.find tabClasses c_rec) with
          | (hopt, tab_m, _) ->
              try matchSignatureGetType (Hashtbl.find_all tab_m nomMethode)
              with
                Not_found | Not_here ->
                  match hopt with
                    | Some(h) -> mmgt_rec h
                    | None -> raise (VC_Error ("pas de methode avec cette signature, dans la classe : " ^ typeClasse ^ if(typeClasse!=c_rec) then (" (ni dans ces classes heritees jusqu'a : " ^ c_rec ^ ")") else ""))
        with Not_found -> raise (VC_Error ("classe non declaree : " ^ c_rec))
      in mmgt_rec typeClasse

(* retourne false s'il n'existe pas de methode avec cette signature *)
let methodeMembreExists typeClasse nomMethode paramMethode tabClasses =
  try (methodeMembreGetType typeClasse nomMethode paramMethode tabClasses ; true)
  with VC_Error(_) -> false


(* retourne le type du champ *)
let champMembreGetType typeClasse nomChamp tabClasses =
  let rec cmgt_rec c_rec =
    try match (Hashtbl.find tabClasses c_rec) with
      | (hopt, _, tab_c) ->
          try (Hashtbl.find tab_c nomChamp)
          with
            Not_found | Not_here ->
              match hopt with
                | Some(h) -> cmgt_rec h
                | None -> raise (VC_Error ("pas de champ avec ce nom, dans la classe : " ^ typeClasse ^ if(typeClasse!=c_rec) then (" (ni dans ces classes heritees jusqu'a : " ^ c_rec ^ ")") else ""))
    with Not_found -> raise (VC_Error ("classe non declaree : " ^ c_rec))
  in cmgt_rec typeClasse

(*
(* retourne un tabVars avec les valeurs communes entre tabVars1 et tabVars2*)
let communVars tabVars1 tabVars2 =
  let res = Hashtbl.create 50
  in
    Hashtbl.iter (fun nomVar1 typeVar1 ->
      Hashtbl.iter (fun nomVar2 typeVar2 ->
        if(nomVar1=nomVar2) then
          if(typeVar1=typeVar2) then
            Hashtbl.add res nomVar1 typeVar1
      ) tabVars2
    ) tabVars1;
    res*)


(* retourne tabVars actualise avec le parametre ajoute comme une variable suplementaire *)
let addVar paireVar tabVars tabClasses =
  match paireVar with
    (nomVar, typeVar) -> Hashtbl.add tabVars nomVar (classeGetType typeVar tabClasses) ; tabVars


(* retourne tabVars actualise avec les parametres ajoutes comme des variables suplementaires *)
let vc_lParam lParam tabVars tabClasses=
  let rec vc_lp lp tabVars_rec =
    match lp with
      | [] -> tabVars_rec
      | p::l -> vc_lp l (addVar p tabVars_rec tabClasses)
  in vc_lp lParam tabVars


(* retourne son type *)
let rec vc_expr expr tabVars tabClasses =
  let rec exprEstInteger e =
    let typeExpr = vc_e e
    in
      if (typeExpr="Integer") then "Integer"
      else raise (VC_Error ("type incorecte pour operation numerique : " ^ typeExpr))
  and vc_e e_rec =
    match e_rec with
      | Id s -> variableGetType s tabVars
      | Cste v -> "Integer"
      | Str s -> "String"
      | Cast (n, e) ->
          let rec cast_rec typeExpr =
            try match (Hashtbl.find tabClasses typeExpr) with (hopt, _, _) ->
              match hopt with
                | None -> raise (VC_Error ("cast invalide car n'herite pas de : " ^ n))
                | Some(h) ->
                    if(n=h) then
                      n
                    else
                      cast_rec h
            with Not_found -> raise (VC_Error ("classe non declaree : " ^ typeExpr))
          in cast_rec (vc_e e)
      | Membre(s1,s2) ->
          if (s1="This") then
            try match (Hashtbl.find tabClasses (Hashtbl.find tabVars "This")) with
              |  (_, _, tabChamps) ->
                  try (Hashtbl.find tabVars s2)
                  with Not_found -> raise (VC_Error ("La classe n'a pas d'attribut : " ^ s2))
            with Not_found -> raise (VC_Error ("This appele en dehors d'une classe !"))
          else if (s1="Super") then
            try match (Hashtbl.find tabClasses (Hashtbl.find tabVars "Super")) with
              |  (_, _, tabChamps) ->
                  try (Hashtbl.find tabVars s2)
                  with Not_found -> raise (VC_Error ("La classe n'a pas d'attribut : " ^ s2))
            with Not_found -> raise (VC_Error ("Super appele en dehors d'une classe ayant un parent !"))
          else raise (VC_Error ("impossible d'acceder a un champ externe (limite a This ou Super) : " ^ s1 ^ "." ^s2))
      | Instance(n,l) ->
          methodeMembreGetType n "0_construct" (vc_lExpr l tabVars tabClasses) tabClasses
      | MethodeExpr(e,s,l) ->
          methodeMembreGetType (vc_e e) s (vc_lExpr l tabVars tabClasses) tabClasses
      | MethodeLocal(n,s,l) ->
          methodeMembreGetType n s (vc_lExpr l tabVars tabClasses) tabClasses
      | Plus(e1,e2) | Moins(e1,e2) | Mult(e1,e2) | Div(e1,e2) ->
          exprEstInteger e1 ; exprEstInteger e2
      | Concat(e1,e2) ->
          let type1 = vc_e e1 in
          let type2 = vc_e e2
          in
            if ((type1="String") && (type2="String")) then "String"
            else raise (VC_Error ("type incorecte pour la concatenation : Concat(" ^ type1 ^ ", " ^ type2 ^")"))
      | MoinsU(e) ->
          exprEstInteger e
  in vc_e expr

(* retourne la liste des types des expressions *)
and vc_lExpr lexpr tabVars tabClasses =
  let rec vc_le le_rec =
    match le_rec with
      | [] -> []
      | e::l -> (vc_expr e tabVars tabClasses)::(vc_le l)
  in vc_le lexpr


(* ne retourne rien *)
let vc_comp comp tabVars tabClasses =
  match comp with (e1,o,e2) -> vc_expr e1 tabVars tabClasses ; vc_expr e2 tabVars tabClasses ; ()


(* retourne tabVars actualise avec les variables declarees ajoutees comme des variables suplementaires *)
let vc_lDecl lDecl tabVars tabClasses=
  let rec vc_ld ld tabVars_rec1 =
    match ld with
      | [] -> tabVars_rec1
      | d::rd ->
          match d with (lVar, t) ->
            let rec vc_lv lv tabVars_rec2 =
              match lv with
                | [] -> tabVars_rec2
                | v::rv -> vc_lv rv (addVar (v,t) tabVars_rec2 tabClasses)
            in vc_ld rd (vc_lv lVar tabVars_rec1)
  in vc_ld lDecl tabVars


(* retourne le type cible *)
let vc_cible cible tabVars tabClasses =
  match cible with
    | Var(s) -> variableGetType s tabVars
    | ChampCible(e, s) -> champMembreGetType (vc_expr e tabVars tabClasses) s tabClasses


(* ne retourne rien *)
let rec vc_instruc instruc tabVars tabClasses =
  let rec vc_i i_rec =
    match i_rec with
      | Expr(e) -> vc_expr e tabVars tabClasses ; ()
      | Bloc(b) -> vc_bloc b tabVars tabClasses
      | Return -> ()
      | IfThenElse(e, i1, i2) -> vc_expr e tabVars tabClasses ; vc_i i1 ; vc_i i2
      | Affectation(c, e) ->
          let typeChamp = vc_cible c tabVars tabClasses in
          let typeExpr = vc_expr e tabVars tabClasses
          in
            if (typeChamp=typeExpr) then ()
            else raise (VC_Error ("le type du champ (" ^ typeChamp ^ ") ne correspond pas a celui de la valeur affecte (" ^ typeExpr ^ ")"))
  in vc_i instruc

(* ne retourne rien *)
and vc_lInstruc linstruc tabVars tabClasses =
  let rec vc_li li_rec =
    match li_rec with
      | [] -> ()
      | i::l -> (vc_instruc i tabVars tabClasses) ; (vc_li l)
  in vc_li linstruc

(* ne retourne rien *)
and vc_bloc bloc tabVars tabClasses =
  match bloc with (ld, li) -> vc_lInstruc li (vc_lDecl ld tabVars tabClasses) tabClasses

(* retourne tabClasses actualises avec la variable et la methode automatiquement cree la cas echeant *)
let vc_champ champ tabVars tabClasses =
  match champ with (a,p) ->
    try
      let thisClasse = (Hashtbl.find tabVars "this")
      in
        match (Hashtbl.find tabClasses thisClasse) with (prev_h, tabMethodes, tabChamps) ->
          match p with (nomChamp, typeChamp) ->
            (if (a) then Hashtbl.add tabMethodes nomChamp ([], typeChamp) ); (*Methode d'acces du nom du champs*)
            Hashtbl.add tabChamps nomChamp typeChamp ;
            Hashtbl.replace tabClasses thisClasse (prev_h, tabMethodes, tabChamps) ; tabClasses
    with Not_found -> raise (VC_Error ("le champ n'est pas dans une classe"))

(* retourne tabClasses actualise avec les variables et leur methode automatiquement cree la cas echeant *)
let vc_lChamp lChamp tabVars tabClasses =
  let rec vc_lc lc tabClasses_rec =
    match lc with
      | [] -> tabClasses_rec
      | ch::l -> vc_lc l (vc_champ ch tabVars tabClasses_rec)
  in vc_lc lChamp tabClasses


(* retourne tabClasses actualise avec la methode cree *)
let vc_methode methode tabVars tabClasses =
  let thisClasse = (Hashtbl.find tabVars "this") in
  let listeTypeParam = List.map (fun (nomParam, typeParam) -> typeParam) methode.listParamMethode in
  let typeRetour = match methode.typeRetour with Some(tr) -> tr | None -> "Void"
  in
    (if (methode.isOverrideMethode) then
      let prevTypeRetour = methodeMembreGetType thisClasse methode.nomMethode listeTypeParam tabClasses
      in
        if(prevTypeRetour=typeRetour) then ()
        else raise (VC_Error ("une methode Override doit avoir la même signature et le meme type de retour"))
    else
      match (Hashtbl.find tabClasses thisClasse) with (prev_h, tabMethodes, prev_tabChamps) ->
        if (methodeMembreExists thisClasse methode.nomMethode listeTypeParam tabClasses) then
          raise (VC_Error ("une methode est deja definit avec cette signature dans ce scope"))
        else Hashtbl.add tabMethodes methode.nomMethode (listeTypeParam, typeRetour);
        Hashtbl.replace tabClasses thisClasse (prev_h, tabMethodes, prev_tabChamps) ; ()
    ) ; vc_bloc methode.corpsMethode (vc_lParam methode.listParamMethode tabVars tabClasses) tabClasses ; tabClasses

(* retourne tabClasses actualise avec les methodes crees *)
let vc_lMethode lMethode tabVars tabClasses =
  let rec vc_lm lm tabClasses_rec =
    match lm with
      | [] -> tabClasses_rec
      | m::l -> vc_lm l (vc_methode m tabVars tabClasses_rec)
  in vc_lm lMethode tabClasses


(* retourne tabClasses actualise *)
let vc_corpsObjet corpsObjet tabVars tabClasses =
  match corpsObjet with (lc, lm) ->
    vc_lMethode lm tabVars (vc_lChamp lc tabVars tabClasses)


(* ne retourne rien *)
let vc_heritage heritage tabVars tabClasses =
  methodeMembreGetType heritage.nomHeritage "0_construct" (vc_lExpr heritage.listArgsHeritage tabVars tabClasses) tabClasses

(*
(* ne retourne rien *)
let vc_prog prog =
  let tabClasses = Hashtbl.create 10 in 
  let tabMethodesInt = Hashtbl.create 1 in
  let tabMethodesStr = Hashtbl.create 2
  in
    Hashtbl.add tabMethodesInt "0_construct" (["Integer"], "Integer");
    Hashtbl.add tabMethodesInt "toString" ([], "String");
    Hashtbl.add tabMethodesStr "0_construct" (["String"], "String");
    Hashtbl.add tabMethodesStr "print" ([], "Void");
    Hashtbl.add tabMethodesStr "println" ([], "Void");
    Hashtbl.add tabClasses ("Integer", None, tabMethodesInt);
    Hashtbl.add tabClasses ("String", None, tabMethodesStr);
    match prog with (l,b) -> vc_bloc b (vc_lobjets l tabClasses)

*)