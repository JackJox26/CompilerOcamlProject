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
	|Str of string
	|Parent of expType
	|Plus of (expType * expType)
	|Moins of (expType * expType)
	|Mult of (expType * expType)
	|Div of (expType * expType)
	|Concat of (expType * expType)
	|MoinsU of expType
	|Comp of (expType * opType * expType)

type typeType = Type of string

type declType = Decl of (string list * typeType)

(*type methodeMembre = *)

(*type membreType = *)

(* type cibleType = 
	 Var of string 
	 | CibleMembre of membreType *) 

type instructionType = 
	Exp of expType
	| Bloc of blocType 
	| IfThenElse of (expType*instructionType*instructionType)
	| Return
	(*| Affectation of (cibleType * expType)*)

and
blocType = 
	BlocLInst of instructionType list 
	| BlocDecl of (declType list * instructionType list)


type paramType = Param of (string * typeType)

type champsType = Champs of (bool * paramType)


 
 type methodeCorps = 
 	Val of (typeType * expType) 
 	| ResultType of (typeType option * blocType)
 


 type methodeType = {
	nomMethode : string;
	listParamMethode : paramType list;
	isOverrideMethode : bool;
	corpsMethode : methodeCorps
 }



type corpsType = Corps of (champsType list * methodeType list)



type heritageType = Heritage of { nomHeritage : string; listArgsHeritage : expType list }



type classeType = 
{	nomClasse : string;
	listParamClasse : paramType list;
	oHeritageClasse : heritageType option;
	oConstructClasse : blocType option;
	corpsClasse : corpsType
}


type objetIsoleType = 
{	
	nomObjetIsole : string;
	oConstructObjetIsole : blocType option;
	corpsObjetIsole : corpsType
}


type objetType = 
	Classe of classeType
	| ObjetIsole of objetIsoleType

	
type progType = Prog of (objetType list * blocType)
