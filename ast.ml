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
	| MethodeLocal of (string * string * exprType list)
	| Plus of (exprType * exprType)
	| Moins of (exprType * exprType)
	| Mult of (exprType * exprType)
	| Div of (exprType * exprType)
	| Concat of (exprType * exprType)
	| MoinsU of exprType


type declType = (string list * typeType)


type cibleType = 
	  Var of string 
	| ChampCible of (exprType * string)


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


type heritageType = { nomHeritage : typeType; listArgsHeritage : exprType list }


type objetType = {
	nomObjet : string;
	isObjetIsole : bool;
	listParamClasse : paramType list;
	oHeritageClasse : heritageType option;
	oConstructObjet : blocType option;
	corpsObjet : corpsType
}


type progType = (objetType list * blocType)


exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string