type op

(*
type expType =
	Id of string
	|Cste of int
	|Parent of expType
	|Plus of (expType * expType)
	|Moins of (expType * expType)
	|Mult of (expType * expType)
	|Div of (expType * expType)
	|PlusU of expType
	|MoinsU of expType
*)

type instructionType : 
	(*Exp of expType
	| *) Bloc of blocType 
	(*| IfThenElse of (expType*instructionType*instructionType) *) 
	| Return

type blocType = 
	BlocLInst of instructionType list 
	| BlocDecl of (decl list * instructionType list) 

type objetType (*= 
	Classe of (string*paramType list*
	ObjetIsole of
	
type classeType = 
{	nom : string;
	listParam : (string*string) list
	OHeritage :
	OConstruct :
}

type paramType = Param of (string * string)
 *)

type progType = Prog of (objetType list * blocType)
	
(* 
type membreType = Attribut of (expType*string)

type cibleType = 
	|Result
	|Var of string
	|Membre of membreType

type objetIsole = 

type objetType = classe | objetIsole 

*)