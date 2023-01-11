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

(* type cibleType = Result | *) 

type instructionType = 
	Exp of expType
	|  Bloc of blocType 
	(*| IfThenElse of (expType*instructionType*instructionType) *) 
	| Return
	| Affectation of (cibleType * expType)

and
blocType = 
	BlocLInst of instructionType list 
	| BlocDecl of (declType list * instructionType list)
	| Empty




type paramType = Param of (string * typeType)

type champsType = Champs of (boolean * paramType)


 (*
 type methodeCorps = Val of (typeType * expType) 
 	| ResultType of (typeType * blocType) 
	| ResultSimple of blocType
 *)

(*
 type methodeType = {
	nom : string;
	listParam : paramType list;
	typeRetour : typeType option;
	isOverride : boolean;
	corps : blocType;
	methodeCorps : methodeCorps
 }
 *)

(*
type corpsType = Corps of (	champsType list * methodeType list)
*)

(*
type heritageType = Heritage of { nom : string; listArgs : string list } | EmptyHeritage
*)

(*
type classeType = 
{	nom : string;
	listParam : paramType list;
	oHeritage : heritageType;
	oConstruct : blocType;
	corps : corpsType
}
*)


type objetIsoleType = 
{	nom : string;
	oConstruct : blocType;
	corps : corpsType
}


type objetType = 
	(*Classe of classeType*)
	| ObjetIsole of objetIsoleType

	
type progType = Prog of (objetType list * blocType)
