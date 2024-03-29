type opType = 
	  PGE
	| PG
	| PPE
	| PP
	| EGAL
	| NEGAL


type typeType = string


type paramType = (string * typeType)


type exprType =
	  Id of string
	| Cste of int
	| Str of string
	| Cast of (string * exprType)
	| Membre of (string * string)
	| Instance of (string * exprType list)
	| MethodeExpr of (exprType * string * exprType list)
	| MethodeClasse of (typeType * string * exprType list)
	| Plus of (exprType * exprType)
	| Moins of (exprType * exprType)
	| Mult of (exprType * exprType)
	| Div of (exprType * exprType)
	| Comp of (exprType * opType * exprType)
	| Concat of (exprType * exprType)
	| MoinsU of exprType


type declType = (string list * typeType)


type cibleType = 
	  Var of string 
	| ChampCible of (string * string)
	| ChampCibleCast of (string * string * string)


type instructionType = 
	  Expr of exprType
	| Bloc of blocType 
	| IfThenElse of (exprType*instructionType*instructionType)
	| Return
	| Affectation of (cibleType * exprType)

and blocType = (declType list * instructionType list)


type champType = (bool * paramType)


type methodeType = {
	nomMethode : string;
	listParamMethode : paramType list;
	isOverrideMethode : bool;
	typeRetour : typeType option;
	corpsMethode : blocType;
}


type corpsType = (champType list * methodeType list)


type objetType = {
	nomObjet : string;
	estClasse : bool;
	listParamClasse : paramType list;
	oNomHeritage : typeType option;
	listArgsHeritage : exprType list;
	oConstructObjet : blocType option;
	corpsObjet : corpsType
}


type progType = (objetType list * blocType)


exception VC_Error of string list * string
exception RUN_Error of string
exception MISC_Error of string