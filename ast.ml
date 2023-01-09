type opType = 
    PGE
    |PG
    |PPE
    |PP
    |EGAL
    |NEGAL


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

type typeType = Type of string

type declType = Decl of (string list * typeType)

type instructionType = 
	(*Exp of expType
	| *) Bloc of blocType 
	(*| IfThenElse of (expType*instructionType*instructionType) *) 
	| Return

and
blocType = 
	BlocLInst of instructionType list 
	| BlocDecl of (declType list * instructionType list) 

type objetType (*= 
	Classe of (string*paramType list*
	ObjetIsole of
	
type classeType = 
{	nom : string;
	listParam : (string*string) list
	OHeritage :
	OConstruct :
	corp : 
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
