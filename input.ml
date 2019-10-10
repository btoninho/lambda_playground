

type ty =
  | Bool
  | Arrow of ty * ty


(** An identifier. *)
type ident = string


type expr =
  | Var of ident
  | Lambda of (ident list * ty option) list * expr
  | Apply of expr * expr
  | Arrow of expr * expr
  | Ascription of expr * ty        
  | True
  | False
  | If of expr * expr * expr


type toplevel =
  | TopLoad of string
  | TopDefinition of ident * expr
  | TopCheck of expr
  | TopEval of expr
