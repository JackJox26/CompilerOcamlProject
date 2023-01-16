let adresses = Hashtbl.create 64
let _ =
    Hashtbl.add adresses "x" "0";
    Hashtbl.add adresses "y" "12"
let kwa = 1

(*
Quand on declare une classe ou un objet, on cree un element de Hashtbl avec le nom de l element et les types de ses membres en resultat (y compris pour les methodes)

Quand on declare une variable d un certain type, on cherche les variables associees a son type et on cree un element de Hashtbl avec le nom de la variable et les positions de ses membres sur la pile en resultat

Quand on fait passer un message on ajoute sur l environnement les noms des membres avec leur position sur la pile, quand la methode se termine on les retire

Quand on fait une affectation, (on calcule la partie droite comme variable temporaire) et on affecte les valeurs aux emplacements de la variable en partie gauche dans la pile
*)

open Ast

let rec traducteur_expType t hash pt =
    match t with 
    Cste(i) ->
        "PUSHI " ^ string_of_int(i)
    |_ -> ""

and traducteur_cibleType t hash pt =
    match t with
    Var(str) ->
         Hashtbl.find hash str
    |_ -> ""
and traducteur_instructionType t hash pt =
    match t with
    Affectation (cible, exp) ->
        (traducteur_expType exp hash pt)
        ^ "\n"
        ^ "STOREG " ^ (traducteur_cibleType cible hash (pt+1))
    |_ -> ""

(*
Tout cramer et reprendre a zero
[Fusionner verifications du contexte et generation de code]
Faut enregistrer les variables visibles a chaque position pour l evaluation locale
Pour les methodes il faut considerer la position relative des variables
Hashtbl forevah
Objets dans le stack
(Poition?) Fonctions membres dans le stack
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
