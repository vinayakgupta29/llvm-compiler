(* High-level Intermediate Representation for VKS *)

open Ast

(* HIR types - simplified and typed version of AST *)

type hir_type =
  | HInt
  | HUint
  | HFloat
  | HBool
  | HChar
  | HString
  | HNil
  | HFunc of hir_type list * hir_type
  | HStruct of string * (string * hir_type) list
  | HEnum of string * (string * hir_type list) list
  | HList of hir_type
  | HArray of hir_type * int
  | HTuple of hir_type list

type hir_binop =
  | HAdd | HSub | HMul | HDiv | HExp | HFloorDiv
  | HAnd | HOr | HEq | HNeq | HLt | HGt | HLeq | HGeq
  | HBitAnd | HBitOr | HBitXor
  | HCons | HConcat

type hir_unop =
  | HNeg | HNot | HBitNot

type hir_expr =
  | HLitInt of int
  | HLitFloat of float
  | HLitBool of bool
  | HLitChar of char
  | HLitString of string
  | HLitNil
  | HVar of string * hir_type
  | HBinOp of hir_binop * hir_expr * hir_expr * hir_type
  | HUnOp of hir_unop * hir_expr * hir_type
  | HCall of hir_expr * hir_expr list * hir_type
  | HIf of hir_expr * hir_expr * hir_expr * hir_type
  | HLet of string * hir_type * hir_expr * hir_expr * hir_type
  | HList of hir_expr list * hir_type
  | HTupleExpr of hir_expr list * hir_type
  | HAccess of hir_expr * hir_expr * hir_type
  | HStructInit of string * (string * hir_expr) list * hir_type
  | HVon of (string * hir_expr) list * hir_type
  | HOutput of hir_expr * hir_type

type hir_stmt =
  | HExpr of hir_expr
  | HLetStmt of string * hir_type * hir_expr
  | HReturn of hir_expr

type hir_func = {
  hir_fname: string;
  hir_params: (string * hir_type) list;
  hir_return_type: hir_type;
  hir_body: hir_expr;
  hir_is_proc: bool;
}

type hir_type_def =
  | HIRStruct of string * (string * hir_type) list
  | HIREnum of string * (string * hir_type list) list

type hir_module_member =
  | HIRFunc of hir_func
  | HIRTypeDef of hir_type_def
  | HIRDirective of string * hir_func
  | HIRLet of string * hir_type * hir_expr

type hir_module = {
  hir_mod_name: string;
  hir_exports: string list;
  hir_members: hir_module_member list;
}

type hir_program =
  | HIRModule of hir_module
  | HIRApp of {
      hir_app_name: string;
      hir_imports: string list;
      hir_app_members: hir_module_member list;
    }

(* Convert AST type to HIR type *)
let rec ast_type_to_hir (t: type_expr) : hir_type =
  match t with
  | TPrim TInt -> HInt
  | TPrim TUint -> HUint
  | TPrim TFloat -> HFloat
  | TPrim TBool -> HBool
  | TPrim TChar -> HChar
  | TPrim TString -> HString
  | TPrim TNil -> HNil
  | TFunc (params, ret) ->
      HFunc (List.map ast_type_to_hir params, ast_type_to_hir ret)
  | TList t -> HList (ast_type_to_hir t)
  | TArray (t, size) ->
      (match size with
       | Some s -> HArray (ast_type_to_hir t, s)
       | None -> HList (ast_type_to_hir t))
  | TTuple types -> HTuple (List.map ast_type_to_hir types)
  | TNamed name -> HStruct (name, [])  (* Will be resolved later *)
  | TVar _ -> HInt  (* Type variables should be resolved by type checker *)
  | _ -> HInt  (* Default fallback *)

(* Convert AST binop to HIR binop *)
let ast_binop_to_hir (op: binop) : hir_binop =
  match op with
  | Add -> HAdd
  | Sub -> HSub
  | Mul -> HMul
  | Div -> HDiv
  | Exp -> HExp
  | FloorDiv -> HFloorDiv
  | And -> HAnd
  | Or -> HOr
  | Eq -> HEq
  | Neq -> HNeq
  | Lt -> HLt
  | Gt -> HGt
  | Leq -> HLeq
  | Geq -> HGeq
  | BitAnd -> HBitAnd
  | BitOr -> HBitOr
  | BitXor -> HBitXor
  | Cons -> HCons
  | Concat -> HConcat
  | CustomOp _ -> HAdd  (* TODO: Handle custom operators *)

(* Convert AST unop to HIR unop *)
let ast_unop_to_hir (op: unop) : hir_unop =
  match op with
  | Neg -> HNeg
  | Not -> HNot
  | BitNot -> HBitNot

(* Get the type of an HIR expression *)
let hir_expr_type (expr: hir_expr) : hir_type =
  match expr with
  | HLitInt _ -> HInt
  | HLitFloat _ -> HFloat
  | HLitBool _ -> HBool
  | HLitChar _ -> HChar
  | HLitString _ -> HString
  | HLitNil -> HNil
  | HVar (_, t) -> t
  | HBinOp (_, _, _, t) -> t
  | HUnOp (_, _, t) -> t
  | HCall (_, _, t) -> t
  | HIf (_, _, _, t) -> t
  | HLet (_, _, _, _, t) -> t
  | HList (_, t) -> t
  | HTupleExpr (_, t) -> t
  | HAccess (_, _, t) -> t
  | HStructInit (_, _, t) -> t
  | HVon (_, t) -> t
  | HOutput (_, t) -> t

(* Convert typed AST expression to HIR expression *)
let rec ast_expr_to_hir (env: Type_checker.type_env) (expr: expr) : hir_expr =
  let expr_type = Type_checker.type_check_expr env expr in
  let hir_type = ast_type_to_hir expr_type in
  
  match expr with
  | ELit (LInt i) -> HLitInt i
  | ELit (LFloat f) -> HLitFloat f
  | ELit (LBool b) -> HLitBool b
  | ELit (LChar c) -> HLitChar c
  | ELit (LString s) -> HLitString s
  | ELit LNil -> HLitNil
  
  | EVar name -> HVar (name, hir_type)
  
  | EBinOp (op, e1, e2) ->
      let h1 = ast_expr_to_hir env e1 in
      let h2 = ast_expr_to_hir env e2 in
      HBinOp (ast_binop_to_hir op, h1, h2, hir_type)
  
  | EUnOp (op, e) ->
      let h = ast_expr_to_hir env e in
      HUnOp (ast_unop_to_hir op, h, hir_type)
  
  | EApp (func, args) ->
      let hfunc = ast_expr_to_hir env func in
      let hargs = List.map (ast_expr_to_hir env) args in
      HCall (hfunc, hargs, hir_type)
  
  | EIf (cond, then_br, else_br) ->
      let hcond = ast_expr_to_hir env cond in
      let hthen = ast_expr_to_hir env then_br in
      let helse = ast_expr_to_hir env else_br in
      HIf (hcond, hthen, helse, hir_type)
  
  | EList elements ->
      let helements = List.map (ast_expr_to_hir env) elements in
      HList (helements, hir_type)
  
  | ETuple elements ->
      let helements = List.map (ast_expr_to_hir env) elements in
      HTupleExpr (helements, hir_type)
  
  | EAccess (arr, idx) ->
      let harr = ast_expr_to_hir env arr in
      let hidx = ast_expr_to_hir env idx in
      HAccess (harr, hidx, hir_type)
  
  | EStruct (name, fields) ->
      let hfields = List.map (fun (fname, fexpr) ->
        (fname, ast_expr_to_hir env fexpr)
      ) fields in
      HStructInit (name, hfields, hir_type)
  
  | ELet (name, _type_opt, value, body) ->
      let hvalue = ast_expr_to_hir env value in
      let value_type = hir_expr_type hvalue in
      let new_env = Type_checker.extend_env env name expr_type in
      let hbody = ast_expr_to_hir new_env body in
      HLet (name, value_type, hvalue, hbody, hir_type)
  
  | EBlock (stmts, ret_opt) ->
      let final_expr = match ret_opt with
        | Some ret_expr -> ast_expr_to_hir env ret_expr
        | None -> HLitNil
      in
      (* Chain statements using HLet with dummy variables for side effects *)
      List.fold_right (fun stmt acc ->
        match stmt with
        | SExpr e -> 
            let he = ast_expr_to_hir env e in
            let ty = hir_expr_type he in
            let acc_ty = hir_expr_type acc in
            HLet ("_", ty, he, acc, acc_ty)
        | SLet (name, _t_opt, value) ->
            let hval = ast_expr_to_hir env value in
            let v_ty = hir_expr_type hval in
            let acc_ty = hir_expr_type acc in
            HLet (name, v_ty, hval, acc, acc_ty)
        | SReturn e -> 
            (* Treat return in block as the final expression *)
            ast_expr_to_hir env e
      ) stmts final_expr
  
  | EVon fields ->
      let hfields = List.map (fun (fname, fexpr) ->
        (fname, ast_expr_to_hir env fexpr)
      ) fields in
      HVon (hfields, hir_type)
  
  | EOutput e ->
      let h = ast_expr_to_hir env e in
      HOutput (h, hir_type)
  
  | _ ->
      (* TODO: Handle remaining expression types *)
      HLitNil

(* Convert AST function to HIR function *)
let ast_func_to_hir (env: Type_checker.type_env) (func: func_def) : hir_func =
  let hparams = List.map (fun p ->
    match p.ptype with
    | Some t -> (p.name, ast_type_to_hir t)
    | None -> (p.name, HInt)  (* Should not happen after type checking *)
  ) func.params in
  
  let param_env = List.fold_left (fun e (name, _t) ->
    Type_checker.extend_env e name (TPrim TInt)  (* Simplified *)
  ) env hparams in
  
  let hbody = ast_expr_to_hir param_env func.body in
  let hret_type = match func.return_type with
    | Some t -> ast_type_to_hir t
    | None -> hir_expr_type hbody
  in
  
  {
    hir_fname = func.fname;
    hir_params = hparams;
    hir_return_type = hret_type;
    hir_body = hbody;
    hir_is_proc = func.is_proc;
  }

(* Convert full AST program to HIR program *)
let ast_to_hir (env: Type_checker.type_env) (prog: program) : hir_program =
  match prog with
  | PModule mod_def ->
      let hir_members = List.map (fun m ->
        match m with
        | MFunc f -> HIRFunc (ast_func_to_hir env f)
        | MDirective (s, f) -> HIRDirective (s, ast_func_to_hir env f)
        | _ -> HIRLet ("unused", HInt, HLitNil)
      ) mod_def.members in
      HIRModule {
        hir_mod_name = mod_def.mod_name;
        hir_exports = mod_def.exports;
        hir_members = hir_members;
      }
  | PApp app_def ->
      let hir_members = List.map (fun m ->
        match m with
        | MFunc f -> HIRFunc (ast_func_to_hir env f)
        | MDirective (s, f) -> HIRDirective (s, ast_func_to_hir env f)
        | _ -> HIRLet ("unused", HInt, HLitNil)
      ) app_def.app_members in
      HIRApp {
        hir_app_name = app_def.app_name;
        hir_imports = List.map (function IUse s | IUseAlias (_, s) -> s) app_def.imports;
        hir_app_members = hir_members;
      }
