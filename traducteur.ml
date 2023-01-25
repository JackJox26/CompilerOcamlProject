(*hashtbl de couples (nom_objet, position_adresse(dans la pile))*)
let adresses_objets = Hashtbl.create 64

(*hashtble de couples (nom_objet, nom_type)*)
let type_objets = Hashtbl.create 64

(*hashtbl de couples (nom_type, position_table_methode(dans la pile))*)
let adresses_table_methode = Hashtbl.create 64

(*hasbtbl de couples (nom_type, hashtbl de couples 
    (nom_champ, position_champ(dans l objet))
)*)
let champs_types = Hashtbl.create 64

(*hashtbl de couples (nom_type, hashtbl de couples 
    (nom_methode, (position_methode(dans la table des methodes de l objet), nom_procedural_label)
)*)
let methodes_types = Hashtbl.create 64

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

(*compteur position courante dans la pile avec methode d'accès et maj*)
let pt = ref 0

let ptp () = pt := !pt + 1
let ptm () = pt := !pt - 1
let gpt () = !pt

(*let _ = pt := !pt + 0*)


(*compteur nb etiquette avec methode d'accès et maj*)
let cmptEti = ref 0

let cmptEtip () = cmptEti := !cmptEti + 1
let cmptEtim () = cmptEti := !cmptEti - 1
let cmptEtig () = !cmptEti


(*compteur nb etiquette de if then else avec methode d'accès et maj (*! différent de cmptEti pour correction bugs *)*)
let cmptITE = ref 0

let cmptITEp () = cmptITE := !cmptITE + 1
let cmptITEm () = cmptITE := !cmptITE - 1
let cmptITEg () = !cmptITE



let genLabelEti() =
    let e in
    e = "e" ^ string_of_int cmptEtig
    cmptEtip;
    e

let genLabelITE() = 
    let i in
    i = "i" ^ string_of_int cmptITEg
    cmptITEp;
    i

let rec traducteur_expType t hash pt =
    match t with 
    Cste(i) ->
        "PUSHI " ^ string_of_int(i) ^ "\n"
    Ident(str) ->
        
    |_ -> ""
and traducteur_cibleType_prefix t hash pt =
    match t with 
    Var(str) ->
        ""
    |_ -> ""
and traducteur_cibleType_suffix t hash pt =
    match t with
    Var(str) ->
        "STOREG" ^ Hashtbl.find hash str ^ "\n"
    |_ -> ""
and traducteur_instructionType t hash pt =
    match t with
    Affectation (cible, exp) ->
        (traducteur_cibleType_prefix cible hash (pt))
        ^ (traducteur_expType exp hash pt)
        ^ (traducteur_cibleType_suffix cible hash (pt))
    |_ -> ""


(*générateur code d'une methode
    paramètre : label = label associé à la méhotde
                methode = arbre ast de la methode
                hashtable = la hashtable du traducteur pour la visibilité des objets*)

let traducteur_methode label methode hashtable = (*TODO*)
    ""
    ^ match methode with
    |None ->
    |_ ->
    

(*générateur code du programme
    paramètre : p = ast du programme
                ?(*A ajoute*) = la hashtable du traducteur pour stockage et acces des positions dans la pile*)

(*ATTENTION : où on écrit ? string ou print ? fichier en paramètre ? + hashtable des hashtables à ajouter en param et appelée lorsque prête*)

let traducteur_prog p (**hashtable à ajouter*) =
    match p with
    |(l,_) -> List.iter (fun o -> "ALLOC " 
        ^ match o.corpsType with 
        |(_,ml) -> add adresses_table_methode o.nomObjet gpt; ptp; 
                add methodes_types o.nomObjet Hashtbl.create 20; 
                let eti =  genLabelEti in 
                add (find methodes_types o.nomObjet) "constructeur" (0,eti) ; 
                traducteur_methode eti o.oConstructObjet (*hshtable à ajouter*); 
                List.iteri (fun i m -> let eti2 = genLabelEti in add (find methodes_types o.nomObjet) m.nomMethode ((i+1),genLabelEti); (*Appel methode genMethode*)) ml; string_of_int (ml.length + 1) ^ "\n" 
        ^ if o.isObjetIsole then "ALLOC "  (*! if pas au bon endroit*)
        ^ match o.corpsType with
            |(cl,_) -> add adresses_objets o.nomObjet gpt; ptp; add type_objets o.nomObjet o.nomObjet; string_of_int (cl.length + 1) ^ "\n" 
                ^ "PUSHG " ^ ... ^ "\n" ^ "PUSHG " ^ ... ^ "\n" ^ ... ^ "STORE 0\n") l
    
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
