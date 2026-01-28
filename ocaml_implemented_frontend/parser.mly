%{
  open Ast
%}

(* Tokens *)
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING UNI_STRING
%token <string> LOWER_ID UPPER_ID
%token <string> HEX_STRING BIN_STRING OCT_STRING
%token <string> CUSTOM_OP

(* Keywords *)
%token FUNC PROC DATA MOD APP CLASS INSTANCE
%token IF ELSE USE OTHERWISE TRUE FALSE IN SUPER VON

(* Directives *)
%token AT_USING AT_START AT_OUTPUT AT_ROOT

(* Type keywords *)
%token TINT TUINT TFLOAT TBOOL TCHAR TUNICHAR TSTRING TUNISTRING TNIL
%token TI8 TI16 TI32 TI64 TI128
%token TU8 TU16 TU32 TU64 TU128
%token TF32 TF64

(* Fixity *)
%token INFIX INFIXL INFIXR

(* Operators *)
%token PLUS MINUS STAR SLASH CARET FLOOR_DIV
%token AND OR NOT EQ NEQ LT GT LEQ GEQ
%token BIT_AND BIT_OR BIT_XOR BIT_NOT
%token CONS CONCAT DOT AT

(* Delimiters *)
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SET_OPEN
%token COMMA SEMI PIPE ARROW TYPE_ARROW ASSIGN INFER_ASSIGN
%token UNDERSCORE LAMBDA ELLIPSIS RANGE_INCLUSIVE

%token EOF

(* Precedence and associativity *)
%right ARROW
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%right CONS
%left CONCAT
%left PLUS MINUS
%left STAR SLASH FLOOR_DIV
%right CARET
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%right NOT BIT_NOT
%left DOT AT
%left LPAREN

%start <Ast.program> program

%%

program:
  | m = module_def EOF { PModule m }
  | a = app_def EOF { PApp a }
  ;

(* Module definition *)
module_def:
  | MOD name = UPPER_ID exports = export_list LBRACE members = list(module_member) RBRACE
    { { mod_name = name; exports = exports; members = members } }
  ;

export_list:
  | LPAREN ids = separated_list(COMMA, identifier) RPAREN { ids }
  | { [] }
  ;

(* App definition *)
app_def:
  | APP name = UPPER_ID LBRACE imports = list(import_stmt) members = list(module_member) RBRACE
    { { app_name = name; imports = imports; app_members = members } }
  ;

import_stmt:
  | USE path = STRING { IUse path }
  | id = UPPER_ID ASSIGN AT_USING LPAREN path = STRING RPAREN { IUseAlias (id, path) }
  | AT_USING LPAREN path = STRING RPAREN { IUse path }
  ;

(* Module members *)
module_member:
  | f = func_def { MFunc f }
  | o = op_def { MOperator o }
  | t = type_def { MType t }
  | c = class_def { MClass c }
  | i = instance_def { MInstance i }
  | id = identifier COLON_OR_INFER t = option(type_annotation) ASSIGN e = expr
    { MLet (id, t, e) }
  | AT_START f = func_rest { MDirective ("start", f "main") } (* Convert @start to main function directive *)
  | PLUS MOD m = module_def { MSubModule m }
  ;

COLON_OR_INFER:
  | COLON { () }
  | INFER_ASSIGN { () }
  ;

COLON:
  | CONS { () }
  ;

(* Function definition *)
func_def:
  | FUNC name = identifier rest = func_rest { rest name }
  | FUNC name = identifier ASSIGN e = expr
    { { fname = name; params = []; return_type = None; body = e; is_proc = false } }
  ;

(* Procedure definition *)
proc_def:
  | PROC name = identifier rest = proc_rest { rest name }
  ;

func_rest:
  | params = param_list ret = option(return_type) body = func_body
    { fun name -> { fname = name; params = params; return_type = ret; body = body; is_proc = false } }
  ;

proc_rest:
  | params = param_list body = func_body
    { fun name -> { fname = name; params = params; return_type = Some (TPrim TNil); body = body; is_proc = true } }
  ;

param_list:
  | LPAREN params = separated_list(COMMA, param) RPAREN { params }
  | { [] }
  ;

param:
  | name = identifier { { name = name; ptype = None } }
  | name = identifier COLON t = type_expr { { name = name; ptype = Some t } }
  | names = separated_nonempty_list(COMMA, identifier) COLON t = type_expr
    { List.hd (List.map (fun n -> { name = n; ptype = Some t }) names) }
  ;

return_type:
  | TYPE_ARROW t = type_expr { t }
  ;

func_body:
  | LBRACE stmts = list(statement) ret = option(return_expr) RBRACE
    { EBlock (stmts, ret) }
  | ARROW e = expr { e }
  | guards = guard_list { EGuard guards }
  ;

return_expr:
  | ARROW e = expr option(SEMI) { e }
  ;

guard_list:
  | LBRACE guards = nonempty_list(guard_clause) RBRACE { guards }
  | guards = nonempty_list(guard_clause) { guards }
  ;

guard_clause:
  | PIPE cond = expr ARROW body = expr option(SEMI)
    { { condition = cond; body = body } }
  ;

(* Operator definition *)
op_def:
  | LPAREN op = operator RPAREN fixity = fixity_spec params = param_list ret = option(return_type) body = func_body
    { { op_symbol = op; fixity = fst fixity; precedence = snd fixity; 
        op_params = params; op_return_type = ret; op_body = body } }
  ;

operator:
  | PLUS { "+" }
  | MINUS { "-" }
  | STAR { "*" }
  | SLASH { "/" }
  | CARET { "^" }
  | op = CUSTOM_OP { op }
  ;

fixity_spec:
  | LPAREN fix = fixity prec = INT RPAREN { (fix, prec) }
  ;

fixity:
  | INFIX { Infix }
  | INFIXL { Infixl }
  | INFIXR { Infixr }
  ;

(* Type definitions *)
type_def:
  | DATA name = UPPER_ID LBRACE fields = separated_list(COMMA, struct_field) RBRACE
    { TDStruct (name, fields) }
  | DATA name = UPPER_ID _type_var = option(UPPER_ID) ASSIGN constructors = separated_nonempty_list(PIPE, enum_constructor)
    { TDEnum (name, constructors) }
  | DATA name = UPPER_ID ASSIGN t = type_expr
    { TDAlias (name, t) }
  ;

struct_field:
  | name = identifier COLON t = type_expr default = option(default_value)
    { { field_name = name; field_type = t; default_value = default } }
  ;

default_value:
  | ASSIGN e = expr { e }
  ;

enum_constructor:
  | name = UPPER_ID types = list(type_expr)
    { (name, types) }
  ;

(* Typeclass definition *)
class_def:
  | CLASS name = UPPER_ID type_var = UPPER_ID LBRACE methods = list(func_signature) RBRACE
    { { class_name = name; type_var = type_var; methods = methods } }
  ;

func_signature:
  | name = identifier params = param_list TYPE_ARROW ret = type_expr
    { { sig_name = name; sig_params = List.map (fun p -> match p.ptype with Some t -> t | None -> TVar "a") params; sig_return = ret } }
  | LPAREN op = operator RPAREN fixity_spec params = param_list TYPE_ARROW ret = type_expr
    { { sig_name = op; sig_params = List.map (fun p -> match p.ptype with Some t -> t | None -> TVar "a") params; sig_return = ret } }
  ;

(* Typeclass instance *)
instance_def:
  | INSTANCE class_name = UPPER_ID inst_type = type_expr LBRACE methods = list(func_def) RBRACE
    { { inst_class = class_name; inst_type = inst_type; inst_methods = methods } }
  ;

(* Type expressions *)
type_expr:
  | t = prim_type { TPrim t }
  | id = UPPER_ID { if String.length id = 1 then TVar id else TNamed id }
  | LPAREN types = separated_list(COMMA, type_expr) RPAREN TYPE_ARROW ret = type_expr
    { TFunc (types, ret) }
  | t = type_expr LBRACK RBRACK { TList t }
  | t = type_expr LBRACK size = INT RBRACK { TArray (t, Some size) }
  | SET_OPEN t = type_expr RBRACE { TSet t }
  | LPAREN types = separated_list(COMMA, type_expr) RPAREN { TTuple types }
  ;

prim_type:
  | TINT { TInt }
  | TUINT { TUint }
  | TFLOAT { TFloat }
  | TBOOL { TBool }
  | TCHAR { TChar }
  | TUNICHAR { TUniChar }
  | TSTRING { TString }
  | TUNISTRING { TUniString }
  | TNIL { TNil }
  | TI8 { TI8 }
  | TI16 { TI16 }
  | TI32 { TI32 }
  | TI64 { TI64 }
  | TI128 { TI128 }
  | TU8 { TU8 }
  | TU16 { TU16 }
  | TU32 { TU32 }
  | TU64 { TU64 }
  | TU128 { TU128 }
  | TF32 { TF32 }
  | TF64 { TF64 }
  ;

type_annotation:
  | COLON t = type_expr { t }
  ;

(* Statements *)
statement:
  | e = expr option(SEMI) { SExpr e }
  | id = identifier t = option(type_annotation) ASSIGN e = expr option(SEMI)
    { SLet (id, t, e) }
  ;

(* Expressions *)
expr:
  | e = simple_expr { e }
  | e1 = expr op = binop e2 = expr { EBinOp (op, e1, e2) }
  | op = unop e = expr { EUnOp (op, e) }
  | func = expr args = arg_list { EApp (func, args) }
  | IF LPAREN cond = expr RPAREN then_branch = block ELSE else_branch = block
    { EIf (cond, then_branch, else_branch) }
  | guards = guard_list { EGuard guards }
  | LAMBDA params = param_list ARROW body = expr { ELambda (List.map (fun p -> (p.name, p.ptype)) params, body) }
  | e = expr AT index = expr { EAccess (e, index) }
  | e = expr DOT field = identifier { EField (e, field) }
  | name = UPPER_ID LBRACE fields = separated_list(COMMA, field_init) RBRACE
    { EStruct (name, fields) }
  | VON LBRACE fields = separated_list(COMMA, field_init) RBRACE
    { EVon fields }
  | AT_OUTPUT LPAREN e = expr RPAREN
    { EOutput e }
  | name = UPPER_ID LPAREN args = separated_list(COMMA, expr) RPAREN
    { EApp (EVar name, args) }
  ;

simple_expr:
  | lit = literal { ELit lit }
  | id = identifier { EVar id }
  | OTHERWISE { ELit (LBool true) }
  | LPAREN e = expr RPAREN { e }
  | LBRACK elements = separated_list(COMMA, expr) RBRACK { EList elements }
  | SET_OPEN elements = separated_list(COMMA, expr) RBRACE { ESet elements }
  | LPAREN elements = separated_list(COMMA, expr) RPAREN
    { match elements with
      | [e] -> e
      | _ -> ETuple elements }
  | block = block { block }
  ;

block:
  | LBRACE stmts = list(statement) ret = option(return_expr) RBRACE
    { EBlock (stmts, ret) }
  ;

arg_list:
  | LPAREN args = separated_list(COMMA, expr) RPAREN { args }
  ;

field_init:
  | name = identifier ASSIGN value = expr { (name, value) }
  ;

(* Binary operators *)
binop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | CARET { Exp }
  | FLOOR_DIV { FloorDiv }
  | AND { And }
  | OR { Or }
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | GT { Gt }
  | LEQ { Leq }
  | GEQ { Geq }
  | BIT_AND { BitAnd }
  | BIT_OR { BitOr }
  | BIT_XOR { BitXor }
  | CONS { Cons }
  | CONCAT { Concat }
  | op = CUSTOM_OP { CustomOp op }
  ;

(* Unary operators *)
unop:
  | MINUS { Neg }
  | NOT { Not }
  | BIT_NOT { BitNot }
  ;

(* Literals *)
literal:
  | i = INT { LInt i }
  | f = FLOAT { LFloat f }
  | TRUE { LBool true }
  | FALSE { LBool false }
  | c = CHAR { LChar c }
  | s = STRING { LString s }
  | TNIL { LNil }
  ;

(* Identifiers *)
identifier:
  | id = LOWER_ID { id }
  | id = UPPER_ID { id }
  ;

%%
