{
open Ast
open Parse
open Lexing
exception Eof
exception Eof_dans_Str
exception Eof_dans_commentaire

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
        "override", OVERRIDE;
        "auto", AUTO;
        "def", DEF;
        "new", NEW;
        "return", RETURN;
        "object", OBJECT
      ]
}

(* abréviation utiles pour les expressions rationnelles *)
let minuscule = ['a'-'z']
let majuscule = ['A'-'Z']
let lettre = ['A'-'Z' 'a'-'z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )


(* l'analyseur lexical est decomposé ici en deux fonctions: l'une qui est
 * specialisée dans la reconnaissance des commentaires à la C, l'autre qui
 * traite les autres tokens à reconnaitre. les deux fonctions vont coopérer.
 * Tout caractere lu dans le fichier doit être reconnu quelque part !
 *)
rule
  token = parse (* a completer *)
   minuscule LC * as id  { 
                        try 
                            Hashtbl.find keyword_table id
                        with
                            Not_found ->                        
                                ID id 
                        }


  |  majuscule LC * as nomclasse    {
                                        NOMCLASSE nomclasse
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
  | '"' {
            string_parse (Buffer.create 17) lexbuf
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
  | eof           {
                       raise (Eof_dans_commentaire) 
                  }
  | _             { comment lexbuf }

and
    string_parse buf = parse
    '"'           {
                        STR (Buffer.contents buf)
                  }
  | '\\'          {
                        eat_next buf lexbuf
                  }
  | eof           {
                       raise (Eof_dans_Str) 
                  }
  | _ as c        {
                        Buffer.add_char buf c;
                        string_parse buf lexbuf
                  }

and
    eat_next buf = parse
    '\\'          {
                        Buffer.add_char buf '\\';
                        string_parse buf lexbuf
                  }
  | 'f'           {
                        Buffer.add_char buf '\012';
                        string_parse buf lexbuf
                  }
  | 'n'           {
                        Buffer.add_char buf '\n';
                        string_parse buf lexbuf
                  }
  | 'r'           {
                        Buffer.add_char buf '\r';
                        string_parse buf lexbuf
                  }
  | 't'           {
                        Buffer.add_char buf '\t';
                        string_parse buf lexbuf
                  }
  | eof           {
                       raise (Eof_dans_Str) 
                  }
  | _ as c        {
                        Buffer.add_char buf c;
                        string_parse buf lexbuf
                  }
