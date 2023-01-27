type opType = 
    PGE
    |PG
    |PPE
    |PP
    |EGAL
    |NEGAL

type typeType = string

type paramType = (string * typeType)

type expType =
	Id of string
	|Cste of int
	|Str of string
	|Cast of (string * expType)
	|Membre of (string * string)
	|Instance of (string * paramType list)
	|MethodeExpr of (expType * string * paramType list)
	|MethodeStatic of (string * string * paramType list)
	|Plus of (expType * expType)
	|Moins of (expType * expType)
	|Mult of (expType * expType)
	|Div of (expType * expType)
	|Concat of (expType * expType)
	|MoinsU of expType

type compType = (expType * opType * expType)

type declType = (string list * typeType)

type cibleType = 
	Var of string 
	| MembreCible of (string * string)
	| MembreCibleCast of (string * string * string)

type instructionType = 
	Exp of expType
	| Bloc of blocType 
	| IfThenElse of (compType*instructionType*instructionType)
	| Return
	| Affectation of (cibleType * expType)

and
blocType = (declType list * instructionType list)


type champsType = (bool * paramType)


 type methodeType = {
	nomMethode : string;
	listParamMethode : paramType list;
	isOverrideMethode : bool;
	typeRetour : typeType option;
	corpsMethode : blocType;
 }



type corpsType = (champsType list * methodeType list)


type heritageType = { nomHeritage : string; listArgsHeritage : expType list }


type objetType = 
{	nomObjet : string;
	isObjetIsole : bool;
	listParamClasse : paramType list;
	oHeritageClasse : heritageType option;
	oConstructObjet : blocType option;
	corpsObjet : corpsType
}
	
type progType = (objetType list * blocType)
