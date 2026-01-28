(* AST type definitions for VKS language *)

type identifier = string

(* Primitive types *)
type prim_type =
  | TInt
  | TUint
  | TFloat
  | TBool
  | TChar
  | TUniChar
  | TString
  | TUniString
  | TNil
  | TI8 | TI16 | TI32 | TI64 | TI128
  | TU8 | TU16 | TU32 | TU64 | TU128
  | TF32 | TF64

(* Type expressions *)
type type_expr =
  | TPrim of prim_type
  | TVar of identifier  (* Generic type variable like A, B *)
  | TNamed of identifier  (* User-defined type *)
  | TFunc of type_expr list * type_expr  (* Function type: args -> return *)
  | TList of type_expr  (* List type *)
  | TArray of type_expr * int option  (* Array with optional size *)
  | TSet of type_expr  (* Set type *)
  | TTuple of type_expr list  (* Tuple type *)

(* Literals *)
type literal =
  | LInt of int
  | LFloat of float
  | LBool of bool
  | LChar of char
  | LString of string
  | LNil

(* Binary operators *)
type binop =
  (* Arithmetic *)
  | Add | Sub | Mul | Div | Exp | FloorDiv
  (* Logical *)
  | And | Or | Eq | Neq | Lt | Gt | Leq | Geq
  (* Bitwise *)
  | BitAnd | BitOr | BitXor
  (* List operations *)
  | Cons | Concat
  (* Custom operator *)
  | CustomOp of string

(* Unary operators *)
type unop =
  | Neg
  | Not
  | BitNot

(* Patterns for pattern matching *)
type pattern =
  | PWildcard  (* _ *)
  | PVar of identifier
  | PLit of literal
  | PCons of pattern * pattern  (* head : tail *)
  | PList of pattern list
  | PTuple of pattern list
  | PConstructor of identifier * pattern list

(* Expressions *)
type expr =
  | ELit of literal
  | EVar of identifier
  | EBinOp of binop * expr * expr
  | EUnOp of unop * expr
  | EApp of expr * expr list  (* Function application *)
  | ELambda of (identifier * type_expr option) list * expr  (* Lambda *)
  | EIf of expr * expr * expr  (* if-else *)
  | EGuard of guard_clause list  (* Guard clauses *)
  | EList of expr list
  | ESet of expr list
  | ETuple of expr list
  | EAccess of expr * expr  (* Array/list access with @ *)
  | EField of expr * identifier  (* Struct field access *)
  | EStruct of identifier * (identifier * expr) list  (* Struct construction *)
  | EMatch of expr * (pattern * expr) list  (* Pattern matching *)
  | EVon of (identifier * expr) list (* Von literal *)
  | EOutput of expr (* @output directive *)
  | ELet of identifier * type_expr option * expr * expr  (* let binding *)
  | EBlock of statement list * expr option  (* Block with statements *)

and guard_clause = {
  condition: expr;
  body: expr;
}

(* Statements *)
and statement =
  | SExpr of expr
  | SLet of identifier * type_expr option * expr
  | SReturn of expr

(* Function/Procedure parameters *)
type param = {
  name: identifier;
  ptype: type_expr option;
}

(* Function definition *)
type func_def = {
  fname: identifier;
  params: param list;
  return_type: type_expr option;
  body: expr;
  is_proc: bool;  (* true for procedures, false for functions *)
}

(* Operator definition *)
type op_def = {
  op_symbol: string;
  fixity: fixity;
  precedence: int;
  op_params: param list;
  op_return_type: type_expr option;
  op_body: expr;
}

and fixity =
  | Infix
  | Infixl
  | Infixr
  | Prefix

(* Struct field *)
type struct_field = {
  field_name: identifier;
  field_type: type_expr;
  default_value: expr option;
}

(* Type definitions *)
type type_def =
  | TDStruct of identifier * struct_field list
  | TDEnum of identifier * (identifier * type_expr list) list  (* Sum type *)
  | TDAlias of identifier * type_expr

(* Typeclass definition *)
type typeclass_def = {
  class_name: identifier;
  type_var: identifier;
  methods: func_signature list;
}

and func_signature = {
  sig_name: identifier;
  sig_params: type_expr list;
  sig_return: type_expr;
}

(* Typeclass instance *)
type instance_def = {
  inst_class: identifier;
  inst_type: type_expr;
  inst_methods: func_def list;
}

(* Module member *)
type module_member =
  | MFunc of func_def
  | MOperator of op_def
  | MType of type_def
  | MClass of typeclass_def
  | MInstance of instance_def
  | MLet of identifier * type_expr option * expr
  | MDirective of string * func_def
  | MSubModule of module_def

(* Module definition *)
and module_def = {
  mod_name: identifier;
  exports: identifier list;  (* Export list *)
  members: module_member list;
}

(* App definition *)
type app_def = {
  app_name: identifier;
  imports: import_stmt list;
  app_members: module_member list;
}

and import_stmt =
  | IUse of string  (* use Math *)
  | IUseAlias of identifier * string  (* M = @using("Math") *)

(* Top-level program *)
type program =
  | PModule of module_def
  | PApp of app_def
