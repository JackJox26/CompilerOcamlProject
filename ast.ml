type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

type expType =
  Id of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| Uminus of expType
| Ifel of compType*expType*expType

and compType =
  Comp of expType*opComp*expType

type declarationType = string*expType

type declarationsType = declarationType list

type progType = declarationsType*expType

exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
