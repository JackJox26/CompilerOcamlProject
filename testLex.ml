open Lexing
open Ast
open Parse

let output token =
    match token with
      CSTE v    -> "Constante entiere: " ^ (string_of_int v)
    | ID id     -> "Ident: " ^ id
    | PPE       -> "operateur <="
    | PP        -> "operateur <"
    | PGE       -> "operateur >="
    | PG        -> "operateur >"
    | EGAL      -> "operateur ="
    | NEGAL     -> "operateur <>"
    | PLUS      -> "operateur +"
    | MOINS     -> "operateur -"
    | TIMES     -> "operateur *"
    | DIV       -> "operateur /"
    | CONCAT    -> "operateur &"
    | POINTVIRGULE -> "symbole ;"
    | DEUXPOINTS-> "symbole :"
    | POINT     -> "symbole ."
    | PARENT_G  -> "symbole ("
    | PARENT_D  -> "symbole )"
    | ACCOLADE_G-> "symbole {"
    | ACCOLADE_D-> "symbole }"
    | AFFECT    -> "symbole :="
    | IF        -> "mot-clef: IF"
    | THEN      -> "mot-clef: THEN"
    | ELSE      -> "mot-clef: ELSE"
    | IS        -> "mot-clef: IS"
    | VAR       -> "mot-clef: VAR"
    | AUTO      -> "mot-clef: AUTO"
    | DEF       -> "mot-clef: DEF"
    | NEW       -> "mot-clef: NEW"
    | OBJECT    -> "mot-clef: OBJECT"
    | RETURN    -> "mot-clef: RETURN"
    | UMOINS ->
       failwith "UMOINS seen in testLex"
    | EOF      -> (* gere avant l'appel a cette fonction, donc impossible *)
       failwith "Should not happen in testLex"
    | _ -> "Unexpected token in testLex"

(* usage: ./testLex nom-de-fichier
 * Applique l'analyseur lexical sur le fichier et imprime les tokens reconnus
 * (sauf ceux non transmis comme les delimiteurs et les commentaires)
 *)
let () =
  if Array.length Sys.argv = 1 then
    print_endline "usage: textLex nom-de-fichier"
  else
    begin
      let file = open_in Sys.argv.(1) in
      let lexbuf = Lexing.from_channel file
      in
      let rec process () =
        match TpLex.token lexbuf with
          EOF -> close_in file
        | tok -> print_endline (output tok); process ()
      in process ();
    end
;;
