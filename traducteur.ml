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
(*Ca ne sert a rien*)
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

(*compteur nb etiquette pour les jumps avec methode d'accès et maj*)
let cmptJump = ref 0

let cmptJumpp () = cmptJump := !cmptJump + 1
let cmptJumpm () = cmptJump := !cmptJump - 1
let cmptJumpg () = !cmptJump

(*Generateur de label d etiquette*)
let genLabelEti() =
    let e = "e" ^ string_of_int (cmptEtig ())
    in
    cmptEtip ();
    e

(*Generateur de label d etiquette de if then else*)
let genLabelITE() = 
    let i = "i" ^ string_of_int (cmptITEg ())
    in
    cmptITEp ();
    i

(*Generateur de label d etiquette de jump*)
let genLabelJump() =
    let j = "j" ^ string_of_int (cmptJumpg ())
    in
    cmptJumpp ();
    j

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
    |Instance (str, expl) ->
        (*TODO Creer/allouer une instance de l objet de type str et mettre son adresse en haut de pile, en appelant son constructeur?*)
        ""
    |MethodeExpr (exp1, str, expl) ->
        (*TODO Call la methode associee avec les arguments passes, methode membre, peut etre de this*)
        ""
    |MethodeClasse (typ, str, expl) ->
        (*TODO Call la methode associee avec les arguments passes, methode statique d objet isole?*)
        ""
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
    |Comp (exp1, op, exp2) ->
        (*Generer le resultat de la partie droite, puis generer le resultat de la partie gauche*)
        let _str = 
            (traducteur_expType exp2 hash)
            ^ (traducteur_expType exp1 hash)
        in
        ptm ();
        ptm (); 
        begin
            (*Comparer les deux parties en fonction de l operateur*)
            match op with
            |PGE -> _str ^ "SUPEQ\n"
            |PG -> _str ^ "SUP\n"
            |PPE -> _str ^ "INFEQ\n"
            |PP -> _str ^ "INF\n"
            |EGAL -> _str ^ "EQUAL\n"
            |NEGAL -> _str ^ "EQUAL\n" ^ "NOT\n"
        end
    |Concat(exp1, exp2) ->
        (*generer le resultat de la partie droite, puis generer le resultat de la partie gauche*)
        let _str =
            (traducteur_expType exp2 hash)
            ^ (traducteur_expType exp1 hash)
        in
        ptm ();
        ptm ();
        _str 
        (*Concatener les deux parties*)
        ^ "CONCAT" ^ "\n"
    |MoinsU(exp) ->
        (*Generer le resultat de la partie droite, puis generer une constante nulle*)
        let _str =
            (traducteur_expType exp hash)
            ^ (traducteur_expType (Cste(0)) hash)
        in
        ptm ();
        ptm ();
        _str
        (*Soustraire la partie droite a 0*)
        ^ "SUB" ^ "\n"
    |_ -> ""
and traducteur_cibleType t exp_d hash =
    match t with 
    Var(str) ->
        (*generer le resultat de la partie droite*)
        let _str =
            (traducteur_expType exp_d hash)
        in
        ptm ();
        _str
        (*Affecter le resultat a la partie gauche*)
        ^ "STOREG " ^ (Hashtbl.find hash.adresse_objets str) ^ "\n"
    |ChampCible(str1, str2) ->
        (*Recuperer la position de l objet dans la pile, puis generer le resultat de la partie droite*)
        let _str =
            (Hashtbl.find hash.adresse_objets str1)
            ^ (traducteur_expType exp_d hash)
        in
        ptm ();
        ptm ();
        _str
        (*Affecter le resultat a la partie gauche*)
        ^ "STORE " ^ (Hashtbl.find (Hashtbl.find hash.champs_types (Hashtbl.find hash.type_objets str1)) str2)
    |ChampCibleCast(str1, str2, str3) ->
        (*Recuperer la position de l objet dans la pile, puis generer le resultat de la partie droite*)
        let _str =
            (Hashtbl.find hash.adresse_objets str2)
            ^ (traducteur_expType exp_d hash)
        in
        ptm ();
        ptm ();
        _str
        (*Affecter le resultat a la partie gauche*)
        ^ "STORE " ^ (Hashtbl.find (Hashtbl.find hash.champs_types (Hashtbl.find hash.type_objets str1)) str3)
    |_ -> ""
and traducteur_instructionType t hash =
    match t with
    Affectation (cible, exp) ->
        (traducteur_cibleType cible exp hash)
    |_ -> ""


(*générateur code d'une définition de methode
    paramètre : label = label associé à la méhotde
                listParam = liste des paramètres
                methode = arbre ast de la methode
                hashtable = la hashtable du traducteur pour la visibilité des objets*)

let rec traducteur_defmethode label listParam methode hashtable = (*TODO*)
    match label with
    |"" -> traducteur_bloc methode.corpsMethode hashtable
    |_ -> label ^ ": " ^ traducteur_bloc methode.corpsMethode hashtable


(*générateur code d'un appel de methode
                                        *)

let traducteur_appelmethode callfunction hashtable =
    let (t, nom, params) = callfunction in 
    String.concat "" (List.iter (fun p -> traducteur_expType p hashtable) params) ^ 
    let (p,label) = Hashtable.find (Hashtable.find hashtable._methodes_types t) nom in
    "DUPN 1\nDUPN 1\nLOAD 0\nLOAD "  ^ p ^ "\nCALL\nPOPN 1\n"

(*générateur code d'une methode
    paramètre : label = label associé à la méhotde
                methode = arbre ast de la methode
                hashtable = la hashtable du traducteur pour la visibilité des objets*)

let traducteur_bloc b hashtable = "" (*TODO*) (*/!\option : match with None*)

(*générateur code du programme
    paramètre : p = ast du programme
                ?(*A ajoute*) = la hashtable du traducteur pour stockage et acces des positions dans la pile*)

(*ATTENTION : où on écrit ? 
            string ou print ? fichier en paramètre ? => string retournee par le programme
            + hashtable des hashtables à ajouter en param et appelée lorsque prête 
            + override et heritage 
            + initialisation avec constructeur =>DONE 
            + compteur position
            + peut-être créer objet structure d'une classe, sans état, dont tous les objets de la classe copieront à leur déclaration*)




let traducteur_prog p hashtbl =
    match p with
    |(l,bl) -> (*CREATION DU CODE DES METHODES*)  
                "JUMP init\n"  ^ String.concat "" (
                List.iter (fun o -> match o.corpsObjet with
                                    |(cl,ml) -> Hashtbl.add hashtbl._methodes_types o.nomObjet Hashtbl.create 20;
                                                
                                                (*constructeur de l'objet*)
                                                let eti =  genLabelEti in 
                                                Hashtbl.add (Hashtbl.find hashtbl._methodes_types o.nomObjet) "constructeur" (0,eti) ;   
                                                
                                                (*constructeur de la super-classe*)
                                                match o.oNomHeritage with 
                                                |None -> (traducteur_defmethode eti o.oConstructObjet o.listParam hashtbl ^)      (*/!\ attention ce n'est pas une méthode mais un bloc dans ast*)
                                                |_ -> (eti ^ ": " ^ traducteur_appelmethode (*param à ajouter*) hashtbl ^ traducteur_defmethode "" o.oConstructObjet o.listParam hashtbl ^) 
                                                                                                
                                                (*méthodes de la super-classe*)
                                                match o.oNomHeritage with
                                                |None -> ("" ^)      
                                                |_ -> (String.concat "" (Hashtbl.iter (fun nm (p,label) -> if not nm = "constructeur" then Hashtbl.add (Hashtbl.find hashtbl._methodes_types o.nomObjet) nm (p,label)) (Hashtbl.find hashtbl._methodes_types o.oNomHeritage)) ^)
                                                
                                                (*chaque méthode de l'objet*)                               
                                                String.concat "" (
                                                List.iteri (fun i m ->  let eti2 = genLabelEti in
                                                                        let cmptMethodeSClasse = ref 0 in 
                                                                        if m.isOverrideMethode then (cmptMethodeSClasse := !cmptMethodeSClasse + 1; let (p,label) = Hashtbl.find (Hashtbl.find hashtbl._methodes_types o.nomObjet) m.nomMethode in Hashtbl.replace (Hashtbl.find hashtbl._methodes_types o.nomObjet) m.nomMethode (p,eti2);)
                                                                        else ( match o.oNomHeritage with
                                                                                |None -> Hashtbl.add (Hashtbl.find hashtbl._methodes_types o.nomObjet) m.nomMethode ((i+1),eti2);     
                                                                                |_ -> Hashtbl.add (Hashtbl.find hashtbl._methodes_types o.nomObjet) m.nomMethode ((i+(Hashtbl.length (Hashtbl.find hashtbl._methodes_types o.oNomHeritage)) - !cmptMethodeSClasse),eti2); 
                                                                                                                                                                                                                                                                        )
                                                                        traducteur_defmethode eti2 m.corpsMethode hashtbl ^
                                                                                                                                                                                                                                                                                            ) ml ) ^ String.concat "" (
                                                
                                                (*Les méthodes des champs auto*)
                                                List.iteri (fun i c -> let (b,(n,t)) = c in if b then let eti3 = genLabelEti in Hashtbl.add (Hashtbl.find hashtbl._methodes_types o.nomObjet) n ((Hashtbl.length (Hashtbl.find hashtbl._methodes_types o.nomObjet)),eti3); traducteur_defmethode eti3 ([],[Membre("this", n)]) hashtbl) cl )
                                                                                                                                                                                                                                                                                                                            ) l ) ^
                (*CREATION DES TABLES DES METHODES*)                                                                                                                            
                "init: " ^ String.concat "" (
                List.iter (fun o -> "ALLOC " ^
                                    match o.corpsObjet with
                                    |(cl,ml) -> let cmptChampsAuto = ref 0 in
                                                List.iter (fun c -> let (b,(n,t)) = c in if b then cmptChampsAuto := !cmptChampsAuto + 1) cl;
                                                
                                                let cmptMethodeOverr = ref 0 in
                                                List.iter (fun m -> if m.isOverrideMethode then cmptMethodeOverr := !cmptMethodeOverr + 1) ml;

                                                match o.oNomHeritage with
                                                |None -> (string_of_int (ml.length + !cmptChampsAuto + 1) ^ "\n" ^)
                                                |_ -> (string_of_int (ml.length + !cmptChampsAuto + (Hashtbl.length (Hashtbl.find hashtbl._methodes_types o.oNomHeritage) ) - !cmptMethodeOverr - 1 + 1) ^ "\n" ^)
                                                
                                                (*Ajout de la position de la table dans le tableau des adresses*)
                                                Hashtbl.add hashtbl._adresse_tables_methodes o.nomObjet gpt; ptp;
                                                
                                                (*Stockage des méthodes*)
                                                String.concat "" (
                                                Hashtbl.iter (fun nm (p,label) ->  "DUPN 1\n" ^
                                                                     "PUSHA " ^ label ^ "\n" ^
                                                                     "STORE " ^ p ^ "\n" ^
                                                                                              ) (Hashtbl.find hashtbl._methodes_types o.nomObjet) ) ^
                                                
                                                (*Maj hashtable des champs*)
                                                Hashtbl.add hashtbl._champs_types o.nomObjet Hashtbl.create 20;
                                                List.iteri(fun i c -> let (b,(n,t)) = c in Hashtbl.add (Hashtbl.find hashtbl._champs_types o.nomObjet) n (i+1)) cl;

                                                
                                                (*CREATION OBJETS ISOLES*)
                                                if not o.estClasse then
                                                    "ALLOC " ^ string_of_int (cl.length + 1) ^ "\n" ^
                                                    Hashtbl.add hashtbl._adresse_objets o.nomObjet gpt; ptp;
                                                    Hashtbl.add hashtbl._type_objets o.nomObjet o.nomObjet;

                                                    (*Affectation table méthodes*)
                                                    "DUPN 1\n" ^ 
                                                    "PUSHG " ^ (Hashtbl.find hashtbl._adresse_tables_methodes o.nomObjet) ^ "\n" ^ 
                                                    "STORE 0\n" ^

                                                    (*Appel constructeur sur l'objet*)
                                                    "DUPN 1\nDUPN 1\nLOAD 0\nLOAD 0\nCALL\nPOPN 1\n" ^                                                                                                                                ) l ) ^
        
                (*BLOC DU PROGRAMME*)
                "START\n" ^
                traducteur_bloc bl hashtbl
        



(* tentative
let traducteur_prog p hash =
    match p with
    |(l,bl) -> (*CREATION DU CODE DES METHODES*)  
                "JUMP init\n"
                ^ String.concat "" (List.map (fun o -> match o.corpsObjet with
                                    |(_,ml) -> add hash.methodes_types o.nomObjet Hashtbl.create 20;
                                                (*constructeur de l'objet*)
                                                let eti =  genLabelEti in 
                                                add (find hash.methodes_types o.nomObjet) "constructeur" (0,eti) ;
                                                traducteur_methode eti o.oConstructObjet hash
                                                
                                                (*chaque méthode de l'objet*)
                                                ^ String.concat "" (List.mapi (fun i m ->   let eti2 = genLabelEti in 
                                                                        add (find hash.methodes_types o.nomObjet) m.nomMethode ((i+1),eti2);
                                                                        traducteur_methode eti2 m.corpsMethode hash
                                                                                                                                        ) ml) 
                                                                                                                                            ) l)
                (*CREATION DES TABLES DES METHODES*)                                                                                                                            
                ^ "init: "
                ^ String.concat "" (List.map (fun o -> "ALLOC " ^
                                    match o.corpsObjet with
                                    |(_,ml) -> string_of_int (ml.length + 1) ^ "\n" ^
                                    "DUPN 1\n" ^
                                    "PUSHA " ^ let (p,label) = (find (find hash.methodes_types o.nomObjet) "constructeur") in label ^ "\n" ^
                                    "STORE 0\n" ^
                                    String.concat "" (List.map (fun m ->  "DUPN 1\n" ^
                                                            "PUSHA " ^ let (p2,label2) = (find (find hash.methodes_types o.nomObjet) m.nomMethode) in label2 ^ "\n" ^
                                                            "STORE " ^ string_of_int (p2) ^ "\n" ^
                                                                                                                                                                    ) ml)
                                                                                                                                                                        ) l)
        
                (*BLOC DU PROGRAMME*)
                ^ "START\n"
                ^ (traducteur_bloc bl hash)
        


*)
(*Ancienne version
    List.iter (fun o -> "ALLOC " (*Pour chaque déclaration de type (classe ou objet isolé), allocation de sa table des méthodes dans le tas*)
    ^ match o.corpsObjet with 
    |(_,ml) -> string_of_int (ml.length + 1) ^ "\n" ^
            add adresses_table_methode o.nomObjet gpt; ptp; 
            add methodes_types o.nomObjet Hashtbl.create 20; 
            let eti =  genLabelEti in (*1ère méthode constructeur*)
            add (find methodes_types o.nomObjet) "constructeur" (0,eti) ;
            let jumpi = genLabelJump in
            "JUMP " ^ jumpi ^ "\n" ^ 
            traducteur_methode eti o.oConstructObjet (*hshtable à ajouter*);  (*Ajout du code du constructeur avec son label*) (*Voir si pas mettre ailleurs/ risque pas de s'exe*)
            jumpi ^ ": " ^ "PUSHG " ^ string_of_int(find adresses_table_methode o.nomObjet) ^ ptp; "\nPUSHA " ^ eti ^ ptp; "\nSTORE 0" ^ ptm; ptm;
            List.iteri (fun i m -> let eti2 = genLabelEti in add (find methodes_types o.nomObjet) m.nomMethode ((i+1),eti2); let jumpi2 = genLabelJump in "\nJUMP " ^ jumpi2 ^ "\n" ^;traducteur_methode eti2 m.corpsMethode (*hshtable à ajouter*); jumpi2 ^ ": " ^"PUSHG " ^ string_of_int(find adresses_table_methode o.nomObjet) ^ ptp; "\nPUSHA " ^ eti2 ^ ptp; "\nSTORE "^ (i+1) ^ ptm; ptm;) ml;  (*Ajout du code de chaque méthode avec son label*)
    if o.isObjetIsole then "\nALLOC "
    ^ match o.corpsObjet with
        |(cl,_) -> add adresses_objets o.nomObjet gpt; ptp; add type_objets o.nomObjet o.nomObjet; string_of_int (cl.length + 1) ^ "\n" 
            ^ "DUPN 1 " ^ "\n" ^ "PUSHG " ^ string_of_int (adresses_table_methode o.nomObjet) ^ "\n" ^ "STORE 0\n" ^ 
            add champs_types o.nomObjet Hashtbl.create 20;
            List.iteri(fun i c -> let (b,(n,t)) = c in add (find champs_types o.nomObjet) n (i+1)) cl; 
            "DUPN 1\nDUPN 1\nLOAD 0\nLOAD 0\nCALL\n" (*Exe le constructeur*)
    ) l ^ "START\n" ^ traducteur_bloc bl (*hshtable à ajouter*) (*Code du bloc*)
*)

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
