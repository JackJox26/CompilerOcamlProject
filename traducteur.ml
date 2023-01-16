let adresses = Hashtbl.create 64
let test x =
    Hashtbl.add adresses "x" 12;
    Hashtbl.add adresses "y" 12;
    print_endline (x)
let kwa = 1

(*
Quand on declare une classe ou un objet, on cree un element de Hashtbl avec le nom de l element et les types de ses membres en resultat (y compris pour les methodes)

Quand on declare une variable d un certain type, on cherche les variables associees a son type et on cree un element de Hashtbl avec le nom de la variable et les positions de ses membres sur la pile en resultat

Quand on fait passer un message on ajoute sur l environnement les noms des membres avec leur position sur la pile, quand la methode se termine on les retire

Quand on fait une affectation, (on calcule la partie droite comme variable temporaire) et on affecte les valeurs aux emplacements de la variable en partie gauche dans la pile
*)
