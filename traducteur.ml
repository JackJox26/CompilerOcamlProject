(*hashtbl de couples (nom_objet, position_adresse(dans la pile))*)
let _adresse_objets = Hashtbl.create 64

(*hashtble de couples (nom_objet, nom_type)*)
let _type_objets = Hashtbl.create 64

(*hashtbl de couples (nom_type, position_table_methode(dans la pile))*)
let _adresse_tables_methodes = Hashtbl.create 64

(*hasbtbl de couples (nom_type, hashtbl de couples 
    (nom_champ, position_champ(dans l objet))
)*)
let _champs_types = Hashtbl.create 64

(*hashtbl de couples (nom_type, hashtbl de couples 
    (nom_methode, (position_methode(dans la table des methodes de l objet), nom_procedural_label)
)*)
let _methodes_types = Hashtbl.create 64

let _ =
    Hashtbl.add _adresse_objets "x" "0";
    Hashtbl.add _adresse_objets "y" "12"
let kwa = 1

type hashtbls = {
    adresse_objets : (string, string) Hashtbl.t;
    type_objets : (string, string) Hashtbl.t;
    adresse_tables_methodes : (string, string) Hashtbl.t;
    champs_types : (string, ((string, string) Hashtbl.t)) Hashtbl.t;
    methodes_types : (string, ((string, (string)) Hashtbl.t)) Hashtbl.t
}

let hashtbl = {
    adresse_objets = _adresse_objets;
    type_objets = _type_objets;
    adresse_tables_methodes = _adresse_tables_methodes;
    champs_types = _champs_types;
    methodes_types = _methodes_types
}

(*
Quand on declare une classe ou un objet, on cree un element de Hashtbl avec le nom de l element et les types de ses membres en resultat (y compris pour les methodes)

Quand on declare une variable d un certain type, on cherche les variables associees a son type et on cree un element de Hashtbl avec le nom de la variable et les positions de ses membres sur la pile en resultat

Quand on fait passer un message on ajoute sur l environnement les noms des membres avec leur position sur la pile, quand la methode se termine on les retire

Quand on fait une affectation, (on calcule la partie droite comme variable temporaire) et on affecte les valeurs aux emplacements de la variable en partie gauche dans la pile
*)

open Ast

let pt = ref 0
(*let _ = pt := !pt + 0*)

let ptp () = pt := !pt + 1
let ptm () = pt := !pt - 1
let gpt () = !pt

let rec traducteur_expType t hash =
    match t with 
    Cste(i) ->
        ptp ();
        "PUSHI " ^ string_of_int(i) ^ "\n"
    |Str(str) ->
        ptp ();
        "PUSHS " ^ str ^ "\n"
    |Id(str) ->
        ptp ();
        "PUSHG " ^ (Hashtbl.find hash.adresse_objets str) ^ "\n"
    |Membre(str1, str2) ->
        let _str =
            (Hashtbl.find hash.adresse_objets str1)
        in
        ptm ();
        _str
        ^ "LOAD " ^ (Hashtbl.find (Hashtbl.find hash.champs_types (Hashtbl.find hash.type_objets str1)) str2)
    |Plus(exp1, exp2) ->
        let _str =
            (traducteur_expType exp1 hash)
            ^ (traducteur_expType exp2 hash)
        in
        ptm ();
        ptm ();
        _str 
        ^ "ADD" ^ "\n"
    |Moins(exp1, exp2) ->
        let _str =
            (traducteur_expType exp2 hash)
            ^ (traducteur_expType exp1 hash)
        in
        ptm ();
        ptm ();
        _str 
        ^ "SUB" ^ "\n"
    |Mult(exp1, exp2) ->
        let _str =
            (traducteur_expType exp1 hash)
            ^ (traducteur_expType exp2 hash)
        in
        ptm ();
        ptm ();
        _str 
        ^ "MUL" ^ "\n"
    |Div(exp1, exp2) ->
        let _str =
            (traducteur_expType exp2 hash)
            ^ (traducteur_expType exp1 hash)
        in
        ptm ();
        ptm ();
        _str 
        ^ "DIV" ^ "\n"
    |Concat(exp1, exp2) ->
        let _str =
            (traducteur_expType exp2 hash)
            ^ (traducteur_expType exp1 hash)
        in
        ptm ();
        ptm ();
        _str 
        ^ "CONCAT" ^ "\n"
    |MoinsU(exp) ->
        let _str =
            (traducteur_expType exp hash)
            ^ (traducteur_expType (Cste(0)) hash)
        in
        ptm ();
        ptm ();
        _str
        ^ "SUB" ^ "\n"
    |_ -> ""
and traducteur_cibleType t exp_d hash =
    match t with 
    Var(str) ->
        let _str =
            (traducteur_expType exp_d hash)
        in
        ptm ();
        _str
        ^ "STOREG " ^ (Hashtbl.find hash.adresse_objets t) ^ "\n"
    |MembreCible(str1, str2) ->
        let _str =
            (Hashtbl.find hash.adresse_objets str1)
            ^ (traducteur_expType exp hash)
        in
        ptm ();
        ptm ();
        _str
        ^ "STORE " ^ (Hashtbl.find (Hashtbl.find hash.champs_types (Hashtbl.find hash.type_objets str1)) str2)
    |_ -> ""
and traducteur_instructionType t hash =
    match t with
    Affectation (cible, exp) ->
        (traducteur_cibleType cible exp hash)
    |_ -> ""

(*
Tout cramer et reprendre a zero
[Fusionner verifications du contexte et generation de code]
Faut enregistrer les variables visibles a chaque position pour l evaluation locale
Pour les methodes il faut considerer la position relative des variables
Hashtbl forevah
Objets dans le stack
(Position?) Fonctions membres dans le stack
sous la forme :
    Pointeur des methodes
    champs classe mere
    champs classe derivee
    champs classe derivee derivee
    ...
sous la forme :
    Constructeur
    methodes classe mere
    methodes classe derivee
    methodes classe derivee derivee
    ...
(les liens vers les methodes dynamiquement raisonnables, on lie les redefinitions)
*)
