open Ast 
open Eval


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




(* initialisation de tabVars *)
let (tabChampsElem : tabVars) = Hashtbl.create 20
let () =
  Hashtbl.add tabChampsElem "nomElem" "String" ;
  Hashtbl.add tabChampsElem "responsable" "String" ;
  Hashtbl.add tabChampsElem "x" "Integer" ;
  Hashtbl.add tabChampsElem "y" "Integer" ;
  Hashtbl.add tabChampsElem "masse" "Float"

(* initialisation de tabMethodes *)
let (tabMethodesInt:tabMethodes) = Hashtbl.create 3
let () =
  Hashtbl.add tabMethodesInt "0_construct" (["Integer"], "Integer") ;
  Hashtbl.add tabMethodesInt "toString" ([], "String") ;
  Hashtbl.add tabMethodesInt "getSigne" ([], "String")

let (tabMethodesStr:tabMethodes) = Hashtbl.create 3
let () =
  Hashtbl.add tabMethodesStr "0_construct" (["String"], "String") ;
  Hashtbl.add tabMethodesStr "print" ([], "Void") ;
  Hashtbl.add tabMethodesStr "println" ([], "Void")

let (tabMethodesFloat:tabMethodes) = Hashtbl.create 2
let () =
  Hashtbl.add tabMethodesFloat "0_construct" (["Integer" ;"Integer"], "Float") ;
  Hashtbl.add tabMethodesFloat "toString" ([], "String")

let (tabMethodesElem:tabMethodes) = Hashtbl.create 4
let () =
  Hashtbl.add tabMethodesElem "0_construct" (["String" ;"String" ;"Integer" ;"Integer" ;"Float"], "Float") ;
  Hashtbl.add tabMethodesElem "distanceFrom" (["Integer" ;"Integer"], "Float") ;
  Hashtbl.add tabMethodesElem "distanceFrom" (["Elem"], "Float") ; (* Pour tester un cas de surcharge *)
  Hashtbl.add tabMethodesElem "toString" ([], "String")

(* initialisation de la tabObjets *)
let (tabObjets:tabObjets) = Hashtbl.create 20
let () =
  Hashtbl.add tabObjets "Integer" (None, tabMethodesInt, (Hashtbl.create 0)) ;
  Hashtbl.add tabObjets "String" (None, tabMethodesStr, (Hashtbl.create 0)) ;
  Hashtbl.add tabObjets "Float" (Some("Integer"), tabMethodesFloat, (Hashtbl.create 0)) ; (* Pour tester un cas d'heritage *)
  Hashtbl.add tabObjets "Elem" (None, tabMethodesElem, tabChampsElem) (* Pour tester un cas avec des champs *)


(* des methodes utiles pour les tests *)
let printType (typeType:typeType) = print_endline typeType

let rec toString_lType (lType:(typeType list)) =
  match lType with
    | [] -> ""
    | [t] -> t
    | t::r -> t ^ ", " ^ toString_lType r

let printTabVars (tabVars:tabVars) = Hashtbl.iter (
  fun nomVar typeVar ->
    print_endline (nomVar^"-> "^typeVar)
  ) tabVars

let printTabMethodes (tabMethodes:tabMethodes) = Hashtbl.iter (
  fun nomMethode (lParamType, typeRetour) ->
    print_endline (nomMethode^"("^(toString_lType lParamType)^") -> " ^ typeRetour)
  ) tabMethodes

let toString_optType optType =
  match optType with None -> "NON" | Some(t) -> t

let printTabObjets (tabObjets:tabObjets) = Hashtbl.iter (
  fun nomObjet (heritageClasseParente, tabMethodesMembres, tabChamps) ->
    print_endline (nomObjet^" extends:"^(toString_optType heritageClasseParente)^" {\nchamps:") ;
    printTabVars tabChamps ;
    print_endline "\nmethodes:" ;
    printTabMethodes tabMethodesMembres ;
    print_endline "}\n"
  ) tabObjets


(* test des prints *)

let () = print_endline "\n --- test_printType ---"
let test_printType = printType "Integer"

let () = print_endline "\n --- test_printTabVars ---"
let test_printTabVars = printTabVars tabChampsElem

let () = print_endline "\n --- test_printTabMethodes ---"
let test_printTabMethodes = printTabMethodes tabMethodesStr

let () = print_endline "\n --- test_printTabObjets ---"
let test_printTabObjets = printTabObjets tabObjets


(* TESTS UNITAIRES DES METHODES DE VERIFICATIONS CONTEXTUELLES *)
let () = print_endline "\n --- test1_variableGetType ---"
let test1_variableGetType = printType(variableGetType "x" tabChampsElem) (* cas present *)
let () = print_endline "\n --- test2_variableGetType ---"
let test2_variableGetType = printType(variableGetType "z" tabChampsElem) (* cas non declare *)

(* TODO ... *)