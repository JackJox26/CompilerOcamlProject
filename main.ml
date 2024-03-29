open Ast
open Lexing

(* lexbuf: correspond au buffer d'entrée associé au programme qu'on traite
 * file_in: descripteur de fichier pour ce programme
 * chan: descripteur de fichier dans lequel on ecrit le code produit pour la
 *       partie compilation. Par défaut le code ira dans le fichier out.txt
 *)
let parse_with_error lexbuf file_in chan =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" file_in
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  try
    (* lance l'analyse syntaxique (qui appellera l'analyse lexicale) en
     * executant au fur et à mesure les actions associées aux productions.
     * L'appel est fait par TpParse.prog (où prog est le non-terminal déclaré
     * comme axiome dans tpParse.mly). 
     * La valeur retournée par la production qui définit l'axiome renvoie une
     * valeur du type progType à définir dans ast.ml.
     *)

    (*AST du code lu*)
    print_newline ();
    Printf.printf "Generation de l ast. . .";
    print_newline ();
    let programme = Parse.prog Lex.token lexbuf in

    (*Effectuer les verifications contextuelles*)
    print_newline ();
    Printf.printf "Verification du contexte. . .";
    print_newline ();
    (*Renvoie une exception si le contexte est incorrect*)
    let () = Eval.vc_prog programme in

    (*Ecrire le code genere dans le fichier*)
    print_newline ();
    Printf.printf "Generation du code. . .";
    print_newline ();
    (*
    let out = Traducteur.traducteur_prog programme hashtbl in
    *)
	(*Ligne temporaire tant que le traducteur ne compile pas assez*)
    Printf.fprintf chan "kwa"
  with (* traite exception général ... *)
    Parse.Error -> (* levée par l'analyseur syntaxique *)
      Printf.fprintf stderr "Syntax error at position %a\n" print_position lexbuf ;
      exit (-1)
  | VC_Error(parcourVC, msg) ->
      Printf.fprintf stderr "Erreur contextuelle: %s\n\n" msg ;
      ( let rec lStringToString ls =
          match ls with
            | [] -> ""
            | [s] -> s
            | s::r -> lStringToString r ^ " > " ^ s
        in Printf.fprintf stderr "Emplacement dans l'arborecense des VC : %s\n\n" (lStringToString parcourVC) ) ;
      exit (-1)
  (*| RUN_Error msg -> (* uniquement pour la version interprete *)
     Printf.fprintf stderr "Erreur à l'execution: %s\n" msg;
     exit (-1)
  | MISC_Error msg -> (* pour l'instant juste erreur lexicale *)
     Printf.fprintf stderr "Error: %s\n" msg;
     exit (-1)*)

let _ =
  let argc = Array.length Sys.argv in
  if (argc = 1) || (argc = 3) || (argc > 4) then
    print_endline "usage: compileur programme [-o fichier-de sortie] "
  else
    begin
      (* si on ne passe pas à l'appel -o nom-de-fichier pour donner un lieu ou
       * ecrire le code produit, on utilise par défaut le fichier "out.txt"
       *)
      let file_out = if ((argc = 4) && (Sys.argv.(2) = "-o")) then Sys.argv.(3) else "out.txt"
      and file_in = Sys.argv.(1) in
      let chan_in = open_in file_in
      and chan_out = open_out file_out in
      let lexbuf = Lexing.from_channel chan_in
      in
      parse_with_error lexbuf file_in chan_out;
      close_in chan_in;
      close_out chan_out
    end
