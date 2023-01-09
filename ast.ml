type opType = 
    PGE
    |PG
    |PPE
    |PP
    |EGAL
    |NEGAL

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



(*
type paramType = Param of (boolean * string * typeType)
 *)

 (*
 type methodeCorps = Val of (typeType * expType) | ResultType of (typeType * blocType) | ResultSimple of blocType
 *)

  (*
 type methodeType = {
	nom : string;
	listParam : paramType list;
	typeRetour : typeType option;
	isOverride : boolean;
	corps : blocType;
	methodeCorps : methodeCorps;
 }
 *)

(*
type corpsType = Corps of (	paramType list * methodeType list)
*)

(*
type heritageType = 
{
	nom : string;
	listArgs : string list;
}
*)

(*
type classeType = 
{	nom : string;
	listParam : paramType list;
	OHeritage : heritageType option;
	OConstruct : blocType option;
	corps : corpsType;
}
*)

(*
type objetIsoleType = 
{	nom : string;
	OConstruct : blocType option;
	corps : corpsType;
}
*)

type objetType (*= 
	Classe of classeType
	| ObjetIsole of objetIsoleType
*)
	
type progType = Prog of (objetType list * blocType)
