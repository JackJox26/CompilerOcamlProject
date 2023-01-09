{
open Ast
open Parse
open Lexing
exception Eof

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

(* Potentiellement utile pour distinguer mots-clef et vrais identificateurs *)
let keyword_table = Hashtbl.create 16

let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "if", IF;
        "then", THEN;
        "else", ELSE;
        "is", IS;
        "var", VAR;
        "class", CLASS;
        "extends", EXTENDS;
        "auto", AUTO;
        "def", DEF;
        "new", NEW;
        "return", RETURN;
        "object", OBJECT
      ]
}

(* abréviation utiles pour les expressions rationnelles *)
let delim_str = '"'
let lettre = ['A'-'Z' 'a'-'z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )
let char_valide = [' '-'~']


(* l'analyseur lexical est decomposé ici en deux fonctions: l'une qui est
 * specialisée dans la reconnaissance des commentaires à la C, l'autre qui
 * traite les autres tokens à reconnaire. les deux fonctions vont coopérer.
 * Tout caractere lu dans le fichier doit être reconnu quelque part !
 *)
rule
  token = parse (* a completer *)
   lettre LC * as id  { 
                        try 
                            Hashtbl.find keyword_table id
                        with
                            Not_found ->                        
                                ID id 
                        }


  | [' ''\t''\r']        { token lexbuf }     (* skip blanks *)
  | '\n'                 { next_line lexbuf; token lexbuf}
  | "/*"		 {
                            (* lance la fonction specialisée dans la
                             * reconnaissance des commentaires
                             *)
                            comment lexbuf
                         }
(* completer avec les autres regles et actions pour renvoyer le prochain token
 * significatif
 *)
  | chiffre * as i { CSTE (int_of_string i) }

(*Detecteur de string*)
  | delim_str ((char_valide *) as _str) delim_str {
                         STR _str
                        }

  | ":=" { AFFECT }

  | '(' { PARENT_G }
  | ')' { PARENT_D }
  | ';' { POINTVIRGULE }
  | ':' { DEUXPOINTS }
  | '.' { POINT }
  | ',' { VIRGULE }

  | '{' { ACCOLADE_G }
  | '}' { ACCOLADE_D }

  | '+' { PLUS }
  | '-' { MOINS }
  | '*' { MUL }
  | '/' { DIV }

  | '>' { OPERATEUR(Ast.PG) }
  | ">=" { OPERATEUR(Ast.PGE) }
  | '<' { OPERATEUR(Ast.PP) }
  | "<=" { OPERATEUR(Ast.PPE) }
  | '=' { OPERATEUR(Ast.EGAL) }
  | "<>" { OPERATEUR(Ast.NEGAL) }

  | '&' { CONCAT }

  | eof { EOF }
  | _ as lxm             { (* On met un message et on essaye de scanner la 
                            * suite. pour détecter le plus d'erreurs possibles
                            * d'un coup. Il faudrait probablement mémoriser
                            * qu'on a rencontré une erreur pour signaler à la
                            * fin que le programme était incorrect.
                            *)
             		   print_endline
                             ("undefined character: " ^ (String.make 1 lxm));
                           token lexbuf
           	         }
and
  comment = parse
  (* completer avec les autres regles qui definissent ce qu'est un commentaire
   * correct
   *)
    "*/"          { (* quand on a reconnu la fin du commentaire, on relance
                     * recursivement l'analyseur lexical pour renvoyer le
                     * prochain token à l'analyseur syntaxique puisqu'on ne
                     * transmet pas les commentaires
                     *)
                     token lexbuf
                  }
  | _             { comment lexbuf }





