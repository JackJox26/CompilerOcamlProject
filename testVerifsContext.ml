open Ast
open Eval

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



(* initialisation de tabVars *)
let tabChampsElem = Hashtbl.create 20;;
Hashtbl.add tabChampsElem "nomElem" "String";
Hashtbl.add tabChampsElem "responsable" "String";
Hashtbl.add tabChampsElem "x" "Integer";
Hashtbl.add tabChampsElem "y" "Integer";
Hashtbl.add tabChampsElem "masse" "Float"

(* initialisation de tabMethodes *)
let tabMethodesInt = Hashtbl.create 2;;
Hashtbl.add tabMethodesInt "0_construct" (["Integer"], "Integer");
Hashtbl.add tabMethodesInt "toString" ([], "String");
Hashtbl.add tabMethodesInt "getSigne" ([], "String");

let tabMethodesStr = Hashtbl.create 3 ;;
Hashtbl.add tabMethodesStr "0_construct" (["String"], "String");
Hashtbl.add tabMethodesStr "print" ([], "Void");
Hashtbl.add tabMethodesStr "println" ([], "Void");

let tabMethodesFloat = Hashtbl.create 2;;
Hashtbl.add tabMethodesFloat "0_construct" (["Integer","Integer"], "Float");
Hashtbl.add tabMethodesFloat "toString" ([], "String");

let tabMethodesElem = Hashtbl.create 4;;
Hashtbl.add tabMethodesFloat "0_construct" (["String","String","Integer","Integer","Float"], "Float");
Hashtbl.add tabMethodesFloat "distanceFrom" (["Integer","Integer"], "Float");
Hashtbl.add tabMethodesFloat "distanceFrom" (["Elem"], "Float"); (* Pour tester un cas de surcharge *)
Hashtbl.add tabMethodesFloat "toString" ([], "String");

(* initialisation de la tabObjets *)
let tabObjets = Hashtbl.create 20;;
Hashtbl.add tabObjets "Integer" (None, tabMethodesInt, (Hashtbl.create 0));
Hashtbl.add tabObjets "String" (None, tabMethodesStr, (Hashtbl.create 0));
Hashtbl.add tabObjets "Float" (Some("Integer"), tabMethodesFloat, (Hashtbl.create 0)); (* Pour tester un cas d'heritage *)
Hashtbl.add tabObjets "Elem" (None, tabMethodesElem, tabChampsElem); (* Pour tester un cas avec des champs *)


(* des methodes utiles pour les tests *)
let printType typeType = print_endline typeType

let printTabVars tabVars = Hashtbl.iter (fun (* TODO *)) tabVars

let printTabMethodes tabMethodes = Hashtbl.iter (fun (* TODO *)) tabMethodes

let printTabObjets tabObjets = Hashtbl.iter (fun (* TODO *)) tabObjets


(* test des prints *)
let test_printType = printType "Integer" ;

let test_printTabVars = printTabVars tabChampsElem ;

let test_printTabMethodes = printTabMethodes tabMethodesStr ;

let test_printTabObjets = printTabObjets tabObjets ;


(* TESTS UNITAIRES DES METHODES DE VERIFICATIONS CONTEXTUELLES *)
let test1_variableGetType = printType(variableGetType "x" tabChampsElem); (* cas present *)
let test2_variableGetType = printType(variableGetType "z" tabChampsElem); (* cas non declare *)

(* TODO ... *)