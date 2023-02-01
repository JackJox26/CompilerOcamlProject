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

    (*nomObjet : (heritageClasseParente, tabMethodesMembres, tabChamps)*)
    type tabObjets = (string, (typeType option * tabMethodes * tabVars)) Hashtbl.t
*)

exception Not_here



(* retourne le type de la variable *)
let variableGetType nomVar tabVars =
  try (Hashtbl.find tabVars nomVar)
  with Not_found -> raise (VC_Error ("variable non declaree : " ^ nomVar))


(* retourne le type de retour de la methode *)
let methodeMembreGetType nomObjet nomMethode paramMethode tabObjets =
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
                  try match (Hashtbl.find tabObjets f) with
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
        try match (Hashtbl.find tabObjets c_rec) with
          | (hopt, tab_m, _) ->
              try matchSignatureGetType (Hashtbl.find_all tab_m nomMethode)
              with
                Not_found | Not_here ->
                  match hopt with
                    | Some(h) -> mmgt_rec h
                    | None -> raise (VC_Error ("pas de methode avec cette signature, dans l'objet' : " ^ nomObjet ^ if(nomObjet!=c_rec) then (" (ni dans ces classes heritees jusqu'a : " ^ c_rec ^ ")") else ""))
        with Not_found -> raise (VC_Error ("objet non declaree : " ^ c_rec))
      in mmgt_rec nomObjet

(* retourne false s'il n'existe pas de methode avec cette signature *)
let methodeMembreExists nomObjet nomMethode paramMethode tabObjets =
  try (methodeMembreGetType nomObjet nomMethode paramMethode tabObjets ; true)
  with VC_Error(_) -> false


(* retourne le type du champ *)
let champMembreGetType nomObjet nomChamp tabObjets =
  let rec cmgt_rec c_rec =
    try match (Hashtbl.find tabObjets c_rec) with
      | (hopt, _, tab_c) ->
          try (Hashtbl.find tab_c nomChamp)
          with
            Not_found | Not_here ->
              match hopt with
                | Some(h) -> cmgt_rec h
                | None -> raise (VC_Error ("pas de champ avec ce nom, dans l'objet' : " ^ nomObjet ^ if(nomObjet!=c_rec) then (" (ni dans ces classes heritees jusqu'a : " ^ c_rec ^ ")") else ""))
    with Not_found -> raise (VC_Error ("objet non declaree : " ^ c_rec))
  in cmgt_rec nomObjet

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


(* retourne le type de la classe (i.e. elle-meme) *)
let vc_type nomObjet tabObjets =
  let tabMethodes =
    try (match Hashtbl.find tabObjets nomObjet with (_, tab_m, _) -> tab_m)
    with Not_found -> raise (VC_Error ("classe non declaree : " ^ nomObjet))
  in
    try (Hashtbl.find tabMethodes "0_construct") ; nomObjet
    with Not_found -> raise (VC_Error ("n'est pas un type car c'est un objet isole : " ^ nomObjet))


(* retourne tabVars actualise avec le parametre ajoute comme une variable suplementaire *)
let addVar paireVar tabVars tabObjets =
  match paireVar with
    (nomVar, typeVar) -> Hashtbl.add tabVars nomVar (vc_type typeVar tabObjets) ; tabVars


(* retourne tabVars actualise avec les parametres ajoutes comme des variables suplementaires *)
let vc_lParam lParam tabVars tabObjets=
  let rec vc_lp lp tabVars_rec =
    match lp with
      | [] -> tabVars_rec
      | p::l -> vc_lp l (addVar p tabVars_rec tabObjets)
  in vc_lp lParam tabVars


(* retourne son type *)
let rec vc_expr expr tabVars tabObjets =
  let typeRes =
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
              try match (Hashtbl.find tabObjets typeExpr) with (hopt, _, _) ->
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
            if (s1="this") then
              try match (Hashtbl.find tabObjets (Hashtbl.find tabVars "this")) with
                |  (_, _, tabChamps) ->
                    try (Hashtbl.find tabVars s2)
                    with Not_found -> raise (VC_Error ("L'objet n'a pas d'attribut : " ^ s2))
              with Not_found -> raise (VC_Error ("this appele en dehors d'un objet !"))
            else if (s1="super") then
              try match (Hashtbl.find tabObjets (Hashtbl.find tabVars "super")) with
                |  (_, _, tabChamps) ->
                    try (Hashtbl.find tabVars s2)
                    with Not_found -> raise (VC_Error ("La classe n'a pas d'attribut : " ^ s2))
              with Not_found -> raise (VC_Error ("super appele en dehors d'une classe ayant un parent !"))
            else raise (VC_Error ("impossible d'acceder a un champ externe (limite a this ou super) : " ^ s1 ^ "." ^s2))
        | Instance(n,l) ->
            methodeMembreGetType n "0_construct" (vc_lExpr l tabVars tabObjets) tabObjets
        | MethodeExpr(e,s,l) ->
            methodeMembreGetType (vc_e e) s (vc_lExpr l tabVars tabObjets) tabObjets
        | MethodeClasse(n,s,l) ->
            methodeMembreGetType n s (vc_lExpr l tabVars tabObjets) tabObjets
        | Plus(e1,e2) | Moins(e1,e2) | Mult(e1,e2) | Div(e1,e2) | Comp(e1,_,e2) ->
            let _ = exprEstInteger e1 in
            exprEstInteger e2
        | Concat(e1,e2) ->
            let type1 = vc_e e1 in
            let type2 = vc_e e2
            in
              if ((type1="String") && (type2="String")) then "String"
              else raise (VC_Error ("type incorecte pour la concatenation : Concat(" ^ type1 ^ ", " ^ type2 ^")"))
        | MoinsU(e) ->
            exprEstInteger e
    in vc_e expr
  in vc_type typeRes tabObjets

(* retourne la liste des types des expressions *)
and vc_lExpr lexpr tabVars tabObjets =
  let rec vc_le le_rec =
    match le_rec with
      | [] -> []
      | e::l -> (vc_expr e tabVars tabObjets)::(vc_le l)
  in vc_le lexpr


(* ne retourne rien *)
let vc_comp comp tabVars tabObjets =
  match comp with (e1,o,e2) ->
    let _ = vc_expr e1 tabVars tabObjets in
    let _ = vc_expr e2 tabVars tabObjets in
    ()


(* retourne tabVars actualise avec les variables declarees ajoutees comme des variables suplementaires *)
let vc_lDecl lDecl tabVars tabObjets=
  let rec vc_ld ld tabVars_rec1 =
    match ld with
      | [] -> tabVars_rec1
      | d::rd ->
          match d with (lVar, t) ->
            let rec vc_lv lv tabVars_rec2 =
              match lv with
                | [] -> tabVars_rec2
                | v::rv -> vc_lv rv (addVar (v,t) tabVars_rec2 tabObjets)
            in vc_ld rd (vc_lv lVar tabVars_rec1)
  in vc_ld lDecl tabVars


(* retourne le type cible *)
let vc_cible cible tabVars tabObjets =
  match cible with
    | Var(s) -> variableGetType s tabVars
    | ChampCible(s1, s2) -> vc_expr (Membre(s1, s2)) tabVars tabObjets
    | ChampCibleCast(n, s1, s2) ->
        Hashtbl.replace tabVars "this" (vc_expr (Cast(n, Id(s1))) tabVars tabObjets) ;
        vc_expr (Membre(s1, s2)) tabVars tabObjets


(* ne retourne rien *)
let rec vc_instruc instruc tabVars tabObjets =
  let rec vc_i i_rec =
    match i_rec with
      | Expr(e) -> let _ = vc_expr e tabVars tabObjets in ()
      | Bloc(b) -> vc_bloc b tabVars tabObjets
      | Return -> ()
      | IfThenElse(e, i1, i2) -> let _ = vc_expr e tabVars tabObjets in vc_i i1 ; vc_i i2
      | Affectation(c, e) ->
          let typeChamp = vc_cible c tabVars tabObjets in
          let typeExpr = vc_expr e tabVars tabObjets
          in
            if (typeChamp=typeExpr) then ()
            else raise (VC_Error ("le type du champ (" ^ typeChamp ^ ") ne correspond pas a celui de la valeur affecte (" ^ typeExpr ^ ")"))
  in vc_i instruc

(* ne retourne rien *)
and vc_lInstruc linstruc tabVars tabObjets =
  let rec vc_li li_rec =
    match li_rec with
      | [] -> ()
      | i::l -> (vc_instruc i tabVars tabObjets) ; (vc_li l)
  in vc_li linstruc

(* ne retourne rien *)
and vc_bloc bloc tabVars tabObjets =
  match bloc with (ld, li) -> vc_lInstruc li (vc_lDecl ld tabVars tabObjets) tabObjets

(* retourne tabObjets actualises avec la variable et la methode automatiquement cree la cas echeant *)
let vc_champ champ tabVars tabObjets =
  match champ with (a,p) ->
    try
      let thisObjet = (Hashtbl.find tabVars "this")
      in
        match (Hashtbl.find tabObjets thisObjet) with (prev_h, tabMethodes, tabChamps) ->
          match p with (nomChamp, typeChamp) ->
            (if (a) then Hashtbl.add tabMethodes nomChamp ([], typeChamp) ); (*Methode d'acces du nom du champs*)
            Hashtbl.add tabChamps nomChamp typeChamp ;
            Hashtbl.replace tabObjets thisObjet (prev_h, tabMethodes, tabChamps) ; tabObjets
    with Not_found -> raise (VC_Error ("le champ n'est pas dans un objet"))

(* retourne tabObjets actualise avec les variables et leur methode automatiquement cree la cas echeant *)
let vc_lChamp lChamp tabVars tabObjets =
  let rec vc_lc lc tabObjets_rec =
    match lc with
      | [] -> tabObjets_rec
      | ch::l -> vc_lc l (vc_champ ch tabVars tabObjets_rec)
  in vc_lc lChamp tabObjets


(* retourne tabObjets actualise avec la methode cree *)
let vc_methode methode tabVars tabObjets =
  let thisObjet = (Hashtbl.find tabVars "this") in
  let listeTypeParam = List.map (fun (nomParam, typeParam) -> typeParam) methode.listParamMethode in
  let typeRetour = match methode.typeRetour with Some(tr) -> tr | None -> "Void"
  in
    (if (methode.isOverrideMethode) then
      let prevTypeRetour = methodeMembreGetType thisObjet methode.nomMethode listeTypeParam tabObjets
      in
        if(prevTypeRetour=typeRetour) then ()
        else raise (VC_Error ("une methode Override doit avoir la même signature et le meme type de retour"))
    else
      match (Hashtbl.find tabObjets thisObjet) with (prev_h, tabMethodes, prev_tabChamps) ->
        if (methodeMembreExists thisObjet methode.nomMethode listeTypeParam tabObjets) then
          raise (VC_Error ("une methode est deja definit avec cette signature dans ce scope"))
        else Hashtbl.add tabMethodes methode.nomMethode (listeTypeParam, typeRetour);
        Hashtbl.replace tabObjets thisObjet (prev_h, tabMethodes, prev_tabChamps) ; ()
    ) ; vc_bloc methode.corpsMethode (vc_lParam methode.listParamMethode tabVars tabObjets) tabObjets ; tabObjets

(* retourne tabObjets actualise avec les methodes crees *)
let vc_lMethode lMethode tabVars tabObjets =
  let rec vc_lm lm tabObjets_rec =
    match lm with
      | [] -> tabObjets_rec
      | m::l -> vc_lm l (vc_methode m tabVars tabObjets_rec)
  in vc_lm lMethode tabObjets


(* retourne tabObjets actualise *)
let vc_corpsObjet corpsObjet tabVars tabObjets =
  match corpsObjet with (lc, lm) ->
    vc_lMethode lm tabVars (vc_lChamp lc tabVars tabObjets)


(* retourne tabObjets actualise *)
let vc_objet objet tabObjets =
  try let _ = (Hashtbl.find tabObjets objet.nomObjet) in raise (VC_Error ("le nom d'un objet ne peut pas etre reutilise : " ^ objet.nomObjet))
  with Not_found ->
    let tabVars = vc_lParam objet.listParamClasse (Hashtbl.create 20) tabObjets in
    let tabMethodes = Hashtbl.create 20
    in
      Hashtbl.add tabVars "this" objet.nomObjet ;
      begin
        if(objet.estClasse) then
          begin
            match objet.oNomHeritage with
              | None -> ()
              | Some(h) -> 
                begin 
                  (if(methodeMembreExists h "0_construct" (vc_lExpr objet.listArgsHeritage tabVars tabObjets) tabObjets) then ()
                  else raise (VC_Error ("impossible d'herite de l'objet avec ces arguments de construction : " ^ h))) ;
                  Hashtbl.add tabVars "super" h
                end
          end ;
          Hashtbl.add tabMethodes "0_construct" ((List.map (fun (_, t) -> t) objet.listParamClasse), objet.nomObjet)
      end ;
      Hashtbl.add tabObjets objet.nomObjet (objet.oNomHeritage, tabMethodes, Hashtbl.create 20) ;
      let nouv_tabObjets = vc_corpsObjet objet.corpsObjet tabVars tabObjets
      in
        match objet.oConstructObjet with
        | None -> nouv_tabObjets
        | Some(constructObjet) -> vc_bloc constructObjet tabVars nouv_tabObjets ; nouv_tabObjets

(* retourne tabObjets actualise *)
let vc_lobjets lObjet tabObjets =
  let rec vc_lo lo tabObjets_rec =
    match lo with
      | [] -> tabObjets_rec
      | o::l -> vc_lo l (vc_objet o tabObjets_rec)
  in vc_lo lObjet tabObjets


(* ne retourne rien *)
let vc_prog prog =
  let tabObjets = Hashtbl.create 10 in
  let tabMethodesInt = Hashtbl.create 2 in
  let tabMethodesStr = Hashtbl.create 3
  in
    Hashtbl.add tabMethodesInt "0_construct" (["Integer"], "Integer");
    Hashtbl.add tabMethodesInt "toString" ([], "String");
    Hashtbl.add tabMethodesStr "0_construct" (["String"], "String");
    Hashtbl.add tabMethodesStr "print" ([], "Void");
    Hashtbl.add tabMethodesStr "println" ([], "Void");
    Hashtbl.add tabObjets "Integer" (None, tabMethodesInt, (Hashtbl.create 0));
    Hashtbl.add tabObjets "String" (None, tabMethodesStr, (Hashtbl.create 0));
    match prog with (l,b) -> vc_bloc b (Hashtbl.create 10) (vc_lobjets l tabObjets)