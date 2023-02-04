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
    (*dont methode 0_constructeur qui correspond  aux constructeurs de la classe*)

    (*nomObjet : (heritageClasseParente, tabMethodesMembres, tabChamps)*)
    type tabObjets = (string, (typeType option * tabMethodes * tabVars)) Hashtbl.t
*)

exception Not_here



(* retourne le type de la variable *)
let vc_variable_GetType nomVar tabVars parcourArboVC = let parcourVC = "vc_variable"::parcourArboVC in
  try (Hashtbl.find tabVars nomVar)
  with Not_found -> raise (VC_Error (parcourVC, "variable non declaree : " ^ nomVar))


(* retourne le type de retour de la methode *)
let vc_methodeMembre_GetType nomObjet nomMethode paramMethode tabObjets parcourArboVC = let parcourVC = "vc_methodeMembre"::parcourArboVC in
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
      let rec mmgt_rec cetteObjet =
        try match (Hashtbl.find tabObjets cetteObjet) with
          | (hopt, tab_m, _) ->
              try matchSignatureGetType (Hashtbl.find_all tab_m nomMethode)
              with
                Not_found | Not_here ->
                  if(nomMethode="0_constructeur") then raise (VC_Error (parcourVC, "les parametres ne correspondent pas au constructeur de "^nomObjet))
                  else match hopt with
                    | Some(h) -> mmgt_rec h
                    | None -> raise (VC_Error (parcourVC, "pas de methode avec cette signature "^nomMethode^"(...), dans l'objet : " ^ nomObjet ^ if(nomObjet!=cetteObjet) then (" (ni dans ses classes heritees jusqu'a : " ^ cetteObjet ^ ")") else ""))
        with Not_found -> raise (VC_Error (parcourVC, "objet non declaree : " ^ cetteObjet))
      in mmgt_rec nomObjet

(* retourne false s'il n'existe pas de methode avec cette signature *)
let vc_methodeMembre_Existe nomObjet nomMethode paramMethode tabObjets parcourArboVC = let parcourVC = "vc_methodeMembre_2"::parcourArboVC in
  try (let _ = vc_methodeMembre_GetType nomObjet nomMethode paramMethode tabObjets parcourVC in true)
  with VC_Error(_) -> false


(* retourne le type du champ *)
let vc_champMembre_GetType nomObjet nomChamp tabObjets parcourArboVC = let parcourVC = "vc_champMembre"::parcourArboVC in
  let rec cmgt_rec cetteObjet =
    try match (Hashtbl.find tabObjets cetteObjet) with
      | (hopt, _, tabChamp) ->
          try (Hashtbl.find tabChamp nomChamp)
          with Not_found ->
            match hopt with
              | Some(h) -> cmgt_rec h
              | None -> raise (VC_Error (parcourVC, "pas de champ avec ce nom : " ^ nomChamp ^ " ; dans l'objet : " ^ nomObjet ^ if(nomObjet!=cetteObjet) then (" (ni dans ses classes heritees jusqu'a : " ^ cetteObjet ^ ")") else ""))
    with Not_found -> raise (VC_Error (parcourVC, "objet non declaree : " ^ cetteObjet))
  in cmgt_rec nomObjet


(* retourne le type de la classe (i.e. elle-meme) *)
let vc_type_GetType nomObjet tabObjets parcourArboVC = let parcourVC = "vc_type"::parcourArboVC in
  let tabMethodes =
    try (match Hashtbl.find tabObjets nomObjet with (_, tab_m, _) -> tab_m)
    with Not_found -> raise (VC_Error (parcourVC, "classe non declaree : " ^ nomObjet))
  in
    try (Hashtbl.find tabMethodes "0_constructeur") ; nomObjet
    with Not_found -> raise (VC_Error (parcourVC, "n'est pas un type car c'est un objet isole : " ^ nomObjet))


(* retourne tabVars actualise avec le parametre ajoute comme une variable suplementaire *)
let addVar_GetNewTabVars paireVar tabVars tabObjets parcourArboVC = let parcourVC = "addVar"::parcourArboVC in
  match paireVar with
    (nomVar, typeVar) -> Hashtbl.add tabVars nomVar (vc_type_GetType typeVar tabObjets parcourVC) ; tabVars


(* retourne tabVars actualise avec les parametres ajoutes comme des variables suplementaires *)
let vc_lparam_GetNewTabVars lParam tabVars tabObjets parcourArboVC = let parcourVC = "vc_lparam"::parcourArboVC in
  let rec vc_lp lp tabVars_rec =
    match lp with
      | [] -> tabVars_rec
      | p::l -> vc_lp l (addVar_GetNewTabVars p tabVars_rec tabObjets parcourVC)
  in vc_lp lParam tabVars


(* retourne son type *)
let rec vc_expr_GetType expr tabVars tabObjets parcourArboVC = let parcourVC = "vc_expr"::parcourArboVC in
  let typeRes =
    let rec exprEstInteger e =
      let typeExpr = vc_e e
      in
        if (typeExpr="Integer") then "Integer"
        else raise (VC_Error (parcourVC, "type incorecte pour operation numerique : " ^ typeExpr))
    and vc_e e_rec =
      match e_rec with
        | Id s -> vc_variable_GetType s tabVars parcourVC
        | Cste v -> "Integer"
        | Str s -> "String"
        | Cast (n, e) ->
            let rec cast_rec typeExpr =
              try match (Hashtbl.find tabObjets typeExpr) with (hopt, _, _) ->
                match hopt with
                  | None -> raise (VC_Error (parcourVC, "cast invalide car n'herite pas de : " ^ n))
                  | Some(h) ->
                      if(n=h) then
                        n
                      else
                        cast_rec h
              with Not_found -> raise (VC_Error (parcourVC, "classe non declaree : " ^ typeExpr))
            in cast_rec (vc_e e)
        | Membre(scope,nomChamp) ->
            if (scope = "this") then
              try vc_champMembre_GetType (Hashtbl.find tabVars "this") nomChamp tabObjets parcourVC
              with Not_found -> raise (VC_Error (parcourVC, "this appele en dehors d'un objet !"))
            else if (scope = "super") then
              try vc_champMembre_GetType (Hashtbl.find tabVars "super") nomChamp tabObjets parcourVC
              with Not_found -> raise (VC_Error (parcourVC, "super appele en dehors d'une classe ayant un parent !"))
            else raise (VC_Error (parcourVC, "impossible d'acceder a un champ externe (limite a this ou super) : " ^ scope ^ "." ^ nomChamp))
        | Instance(n,l) ->
            vc_methodeMembre_GetType n "0_constructeur" (vc_lExpr_GetListeTypes l tabVars tabObjets parcourVC) tabObjets parcourVC
        | MethodeExpr(e,s,l) ->
            vc_methodeMembre_GetType (vc_e e) s (vc_lExpr_GetListeTypes l tabVars tabObjets parcourVC) tabObjets parcourVC
        | MethodeClasse(n,s,l) ->
            vc_methodeMembre_GetType n s (vc_lExpr_GetListeTypes l tabVars tabObjets parcourVC) tabObjets parcourVC
        | Plus(e1,e2) | Moins(e1,e2) | Mult(e1,e2) | Div(e1,e2) | Comp(e1,_,e2) ->
            let _ = exprEstInteger e1 in
            exprEstInteger e2
        | Concat(e1,e2) ->
            let type1 = vc_e e1 in
            let type2 = vc_e e2
            in
              if ((type1="String") && (type2="String")) then "String"
              else raise (VC_Error (parcourVC, "type incorecte pour la concatenation : Concat(" ^ type1 ^ ", " ^ type2 ^")"))
        | MoinsU(e) ->
            exprEstInteger e
    in vc_e expr
  in vc_type_GetType typeRes tabObjets parcourVC

(* retourne la liste des types des expressions *)
and vc_lExpr_GetListeTypes lexpr tabVars tabObjets parcourArboVC = let parcourVC = "vc_lExpr"::parcourArboVC in
  let rec vc_le le_rec =
    match le_rec with
      | [] -> []
      | e::l -> (vc_expr_GetType e tabVars tabObjets parcourVC)::(vc_le l)
  in vc_le lexpr


(* retourne tabVars actualise avec les variables declarees ajoutees comme des variables suplementaires *)
let vc_lDecl_GetNewTabVars lDecl tabVars tabObjets parcourArboVC = let parcourVC = "vc_lDecl"::parcourArboVC in
  let rec vc_ld ld tabVars_rec1 =
    match ld with
      | [] -> tabVars_rec1
      | d::rd ->
          match d with (lVar, t) ->
            let rec vc_lv lv tabVars_rec2 =
              match lv with
                | [] -> tabVars_rec2
                | v::rv -> vc_lv rv (addVar_GetNewTabVars (v,t) tabVars_rec2 tabObjets parcourVC)
            in vc_ld rd (vc_lv lVar tabVars_rec1)
  in vc_ld lDecl tabVars


(* retourne le type cible *)
let vc_cible_GetType cible tabVars tabObjets parcourArboVC = let parcourVC = "vc_cible"::parcourArboVC in
  match cible with
    | Var(s) -> vc_variable_GetType s tabVars parcourVC
    | ChampCible(s1, s2) -> vc_expr_GetType (Membre(s1, s2)) tabVars tabObjets parcourVC
    | ChampCibleCast(n, s1, s2) ->
        Hashtbl.replace tabVars "this" (vc_expr_GetType (Cast(n, Id(s1))) tabVars tabObjets parcourVC) ;
        vc_expr_GetType (Membre(s1, s2)) tabVars tabObjets parcourVC


(* return true si le resultat a ete affecte *)
let rec vc_instruc_ResultAffecte instruc tabVars tabObjets parcourArboVC = let parcourVC = "vc_instruc"::parcourArboVC in
  let rec vc_i i_rec =
    match i_rec with
      | Expr(e) -> let _ = vc_expr_GetType e tabVars tabObjets parcourVC in false
      | Bloc(b) -> vc_bloc_ResultAffecte b tabVars tabObjets parcourVC
      | Return -> false
      | IfThenElse(e, i1, i2) -> let _ = vc_expr_GetType e tabVars tabObjets parcourVC in (vc_i i1) && (vc_i i2)
      | Affectation(c, e) ->
          let typeChamp = vc_cible_GetType c tabVars tabObjets parcourVC in
          let typeExpr = vc_expr_GetType e tabVars tabObjets parcourVC
          in
            let rec typeCompatible a b =
              if(a=b) then true
              else
                try match (Hashtbl.find tabObjets b) with (h_opt, _, _) ->
                  match h_opt with
                    | Some(h) -> typeCompatible a h
                    | None -> false
                with Not_found -> false
            in
              if (typeCompatible typeChamp typeExpr) then match c with Var(s) -> (s="result") | _ -> false
              else raise (VC_Error (parcourVC, "le type du champ (" ^ typeChamp ^ ") n'est pas compatible modulo heritage avec celui de la valeur affecte (" ^ typeExpr ^ ")"))
  in vc_i instruc

(* return true si le resultat a ete affecte *)
and vc_lInstruc_ResultAffecte linstruc tabVars tabObjets parcourArboVC = let parcourVC = "vc_lInstruc_ResultAffecte"::parcourArboVC in
  let rec vc_li li_rec =
    match li_rec with
      | [] -> false
      | i::l -> (vc_instruc_ResultAffecte i tabVars tabObjets parcourVC) || (vc_li l)
  in vc_li linstruc

(* return true si le resultat a ete affecte *)
and vc_bloc_ResultAffecte bloc tabVars tabObjets parcourArboVC = let parcourVC = "vc_bloc"::parcourArboVC in
  match bloc with (ld, li) -> vc_lInstruc_ResultAffecte li (vc_lDecl_GetNewTabVars ld tabVars tabObjets parcourVC) tabObjets parcourVC


(* retourne tabObjets actualise avec la methode cree *)
let addMethode_GetNewTabObjets methode tabVars tabObjets parcourArboVC = let parcourVC = "addMethode"::parcourArboVC in
  let thisObjet = (Hashtbl.find tabVars "this") in
  let listeTypeParam = List.map (fun (nomParam, typeParam) -> typeParam) methode.listParamMethode in
  let typeRetour = match methode.typeRetour with Some(tr) -> vc_type_GetType tr tabObjets parcourVC | None -> "Void"
  in
    (if (methode.isOverrideMethode) then
      try let classeParente = (Hashtbl.find tabVars "super")
      in
        let prevTypeRetour = vc_methodeMembre_GetType classeParente methode.nomMethode listeTypeParam tabObjets parcourVC
        in
          if(prevTypeRetour=typeRetour) then ()
          else raise (VC_Error (parcourVC, "une methode Override doit avoir la même signature et le meme type de retour"))
      with Not_found -> raise(VC_Error(parcourVC, "une methode Override doit etre defini pour une classe ayant une classe parente"))
    else
      match (Hashtbl.find tabObjets thisObjet) with (prev_h, tabMethodes, prev_tabChamps) ->
        if (vc_methodeMembre_Existe thisObjet methode.nomMethode listeTypeParam tabObjets parcourVC) then
          raise (VC_Error (parcourVC, "une methode est deja definit avec cette signature dans ce scope"))
        else
          (Hashtbl.add tabMethodes methode.nomMethode (listeTypeParam, typeRetour); Hashtbl.replace tabObjets thisObjet (prev_h, tabMethodes, prev_tabChamps))
    ) ; tabObjets

(* ne retourne rien *)
let vc_methode methode tabVars tabObjets parcourArboVC = let parcourVC = ("vc_methode("^methode.nomMethode^")")::parcourArboVC in
  match methode.typeRetour with 
    | Some(typeResultAttendu) ->
        Hashtbl.add tabVars "result" typeResultAttendu ;
        if (vc_bloc_ResultAffecte methode.corpsMethode (vc_lparam_GetNewTabVars methode.listParamMethode tabVars tabObjets parcourVC) tabObjets parcourVC) then ()
        else raise(VC_Error(parcourVC, "pas de resultat retourne alors que le type : "^typeResultAttendu^" ; est attendu dans la methode : "^methode.nomMethode))
    | None ->
        let _ = vc_bloc_ResultAffecte methode.corpsMethode (vc_lparam_GetNewTabVars methode.listParamMethode tabVars tabObjets parcourVC) tabObjets parcourVC in ()
  

(* retourne tabObjets actualise avec les methodes crees *)
let vc_lMethode_GetNewTabObjets lMethode tabVars tabObjets parcourArboVC = let parcourVC = "vc_lMethode"::parcourArboVC in
  let rec add_lm lm tabObjets_rec =
    match lm with
      | [] -> tabObjets_rec
      | m::l -> add_lm l (addMethode_GetNewTabObjets m tabVars tabObjets_rec parcourVC)
  in
    let nouv_tabObjets = add_lm lMethode tabObjets
    in
      let rec vc_lm lm =
        match lm with
          | [] -> ()
          | m::l -> (vc_methode m tabVars nouv_tabObjets parcourVC) ; vc_lm l 
      in vc_lm lMethode ; nouv_tabObjets


(* retourne tabObjets actualises avec la variable et la methode automatiquement cree la cas echeant *)
let vc_champ_GetNewTabObjets champ tabVars tabObjets parcourArboVC =
  match champ with (a,p) ->
    match p with (nomChamp, typeChamp) ->
      let parcourVC = ("vc_champ("^nomChamp^")")::parcourArboVC in
      try
        let thisObjet = (Hashtbl.find tabVars "this")
        in
          (if(nomChamp="result" || nomChamp="this" || nomChamp="super" || nomChamp="return") then raise (VC_Error(parcourVC, "Nom de champ invalide : "^nomChamp)));
            let nouv_tabObjets = 
              if (a) then
                addMethode_GetNewTabObjets {nomMethode=nomChamp ; listParamMethode=[] ; isOverrideMethode=false ; typeRetour=Some(typeChamp) ; corpsMethode=([],[Affectation(Var("result"),Membre("this",nomChamp))]) } tabVars tabObjets parcourVC (*Methode d'acces du nom du champs*)
              else tabObjets
            in
              match (Hashtbl.find nouv_tabObjets thisObjet) with (prev_h, tabMethodes, tabChamps) ->
                Hashtbl.add tabChamps nomChamp typeChamp ;
                Hashtbl.replace nouv_tabObjets thisObjet (prev_h, tabMethodes, tabChamps) ; nouv_tabObjets
      with Not_found -> raise (VC_Error (parcourVC, "le champ "^nomChamp^" n'est pas appele depuis un objet"))

(* retourne tabObjets actualise avec les variables et leur methode automatiquement cree la cas echeant *)
let vc_lChamp_GetNewTabObjets lChamp tabVars tabObjets parcourArboVC = let parcourVC = "vc_lChamp"::parcourArboVC in
  let rec vc_lc lc tabObjets_rec =
    match lc with
      | [] -> tabObjets_rec
      | ch::l -> vc_lc l (vc_champ_GetNewTabObjets ch tabVars tabObjets_rec parcourVC)
  in vc_lc lChamp tabObjets


(* retourne tabObjets actualise *)
let vc_corpsObjet_GetNewTabObjets corpsObjet tabVars tabObjets parcourArboVC = let parcourVC = "vc_corpsObjet"::parcourArboVC in
  match corpsObjet with (lc, lm) ->
    vc_lMethode_GetNewTabObjets lm tabVars (vc_lChamp_GetNewTabObjets lc tabVars tabObjets parcourVC) parcourVC


(* retourne tabObjets actualise *)
let vc_objet_GetNewTabObjets objet tabObjets parcourArboVC = let parcourVC = ("vc_objet("^objet.nomObjet^")")::parcourArboVC in
  try let _ = (Hashtbl.find tabObjets objet.nomObjet) in raise (VC_Error (parcourVC, "le nom d'un objet ne peut pas etre reutilise : " ^ objet.nomObjet))
  with Not_found ->
    let tabVars = vc_lparam_GetNewTabVars objet.listParamClasse (Hashtbl.create 20) tabObjets parcourVC in
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
                  (if(vc_methodeMembre_Existe h "0_constructeur" (vc_lExpr_GetListeTypes objet.listArgsHeritage tabVars tabObjets parcourVC) tabObjets parcourVC) then ()
                  else raise (VC_Error (parcourVC, "impossible d'herite de l'objet avec ces arguments de construction : " ^ h))) ;
                  Hashtbl.add tabVars "super" h
                end
          end ;
          Hashtbl.add tabMethodes "0_constructeur" ((List.map (fun (_, t) -> t) objet.listParamClasse), objet.nomObjet)
      end ;
      Hashtbl.add tabObjets objet.nomObjet (objet.oNomHeritage, tabMethodes, Hashtbl.create 20) ;
      let nouv_tabObjets = vc_corpsObjet_GetNewTabObjets objet.corpsObjet tabVars tabObjets parcourVC
      in
        match objet.oConstructObjet with
        | None -> nouv_tabObjets
        | Some(constructObjet) -> let _ = vc_bloc_ResultAffecte constructObjet tabVars nouv_tabObjets parcourVC in nouv_tabObjets

(* retourne tabObjets actualise *)
let vc_lobjets lObjet tabObjets = let parcourVC = ["vc_lobjets"] in
  let rec vc_lo lo tabObjets_rec =
    match lo with
      | [] -> tabObjets_rec
      | o::l -> vc_lo l (vc_objet_GetNewTabObjets o tabObjets_rec parcourVC)
  in vc_lo lObjet tabObjets


(* ne retourne rien *)
let vc_prog prog =
  let tabObjets = Hashtbl.create 10 in
  let tabMethodesVoid = Hashtbl.create 1 in
  let tabMethodesInt = Hashtbl.create 2 in
  let tabMethodesStr = Hashtbl.create 3
  in
    Hashtbl.add tabMethodesVoid "0_constructeur" ([], "Void");
    Hashtbl.add tabMethodesInt "0_constructeur" (["Integer"], "Integer");
    Hashtbl.add tabMethodesInt "toString" ([], "String");
    Hashtbl.add tabMethodesStr "0_constructeur" (["String"], "String");
    Hashtbl.add tabMethodesStr "print" ([], "Void");
    Hashtbl.add tabMethodesStr "println" ([], "Void");
    Hashtbl.add tabObjets "Void" (None, tabMethodesVoid, (Hashtbl.create 0));
    Hashtbl.add tabObjets "Integer" (None, tabMethodesInt, (Hashtbl.create 0));
    Hashtbl.add tabObjets "String" (None, tabMethodesStr, (Hashtbl.create 0));
    match prog with (l,b) -> let _ = vc_bloc_ResultAffecte b (Hashtbl.create 10) (vc_lobjets l tabObjets) [] in ()