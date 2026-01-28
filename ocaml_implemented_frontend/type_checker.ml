(* Type system for VKS language *)

open Ast

(* Type environment for type checking *)
type type_env = (identifier * type_expr) list

exception TypeError of string

(* Create a new empty type environment *)
let empty_env : type_env = []

(* Prelude environment with built-in functions *)
let prelude_env : type_env = []

(* Add a binding to the environment *)
let extend_env (env: type_env) (name: identifier) (t: type_expr) : type_env =
  (name, t) :: env

(* Lookup a type in the environment *)
let rec lookup_env (env: type_env) (name: identifier) : type_expr option =
  match env with
  | [] -> None
  | (n, t) :: rest ->
      if n = name then Some t
      else lookup_env rest name

(* Check if two types are equal *)
let rec types_equal (t1: type_expr) (t2: type_expr) : bool =
  match (t1, t2) with
  | (TPrim p1, TPrim p2) -> p1 = p2
  | (TVar v1, TVar v2) -> v1 = v2
  | (TNamed n1, TNamed n2) -> n1 = n2
  | (TFunc (args1, ret1), TFunc (args2, ret2)) ->
      List.length args1 = List.length args2 &&
      List.for_all2 types_equal args1 args2 &&
      types_equal ret1 ret2
  | (TList t1, TList t2) -> types_equal t1 t2
  | (TArray (t1, s1), TArray (t2, s2)) -> types_equal t1 t2 && s1 = s2
  | (TSet t1, TSet t2) -> types_equal t1 t2
  | (TTuple ts1, TTuple ts2) ->
      List.length ts1 = List.length ts2 &&
      List.for_all2 types_equal ts1 ts2
  | _ -> false

(* Get the type of a literal *)
let type_of_literal (lit: literal) : type_expr =
  match lit with
  | LInt _ -> TPrim TInt
  | LFloat _ -> TPrim TFloat
  | LBool _ -> TPrim TBool
  | LChar _ -> TPrim TChar
  | LString _ -> TPrim TString
  | LNil -> TPrim TNil

(* Get the return type of a binary operator *)
let type_of_binop (op: binop) (t1: type_expr) (t2: type_expr) : type_expr =
  match op with
  | Add | Sub | Mul | Div | FloorDiv ->
      if types_equal t1 t2 then t1
      else raise (TypeError (Printf.sprintf "Type mismatch in arithmetic operation"))
  | Exp ->
      (* Exponentiation only works on integers *)
      if types_equal t1 (TPrim TInt) && types_equal t2 (TPrim TInt) then
        TPrim TInt
      else raise (TypeError "Exponentiation requires integer operands")
  | And | Or ->
      if types_equal t1 (TPrim TBool) && types_equal t2 (TPrim TBool) then
        TPrim TBool
      else raise (TypeError "Logical operators require boolean operands")
  | Eq | Neq | Lt | Gt | Leq | Geq ->
      if types_equal t1 t2 then TPrim TBool
      else raise (TypeError "Comparison requires same types")
  | BitAnd | BitOr | BitXor ->
      if types_equal t1 (TPrim TBool) && types_equal t2 (TPrim TBool) then
        TPrim TBool
      else raise (TypeError "Bitwise operators require boolean operands")
  | Cons ->
      (* head : tail where tail is a list *)
      (match t2 with
       | TList elem_type ->
           if types_equal t1 elem_type then t2
           else raise (TypeError "Cons operator type mismatch")
       | _ -> raise (TypeError "Cons operator requires list as second argument"))
  | Concat ->
      (* List concatenation *)
      if types_equal t1 t2 then t1
      else raise (TypeError "Concat requires same list types")
  | CustomOp _ ->
      (* Custom operators need to be looked up in environment *)
      raise (TypeError "Custom operator type checking not yet implemented")

(* Type check an expression *)
let rec type_check_expr (env: type_env) (expr: expr) : type_expr =
  match expr with
  | ELit lit -> type_of_literal lit
  
  | EVar name ->
      (match lookup_env env name with
       | Some t -> t
       | None -> raise (TypeError (Printf.sprintf "Undefined variable: %s" name)))
  
  | EBinOp (op, e1, e2) ->
      let t1 = type_check_expr env e1 in
      let t2 = type_check_expr env e2 in
      type_of_binop op t1 t2
  
  | EUnOp (Neg, e) ->
      let t = type_check_expr env e in
      (match t with
       | TPrim TInt | TPrim TFloat -> t
       | _ -> raise (TypeError "Negation requires numeric type"))
  
  | EUnOp (Not, e) ->
      let t = type_check_expr env e in
      if types_equal t (TPrim TBool) then TPrim TBool
      else raise (TypeError "Logical not requires boolean type")
  
  | EUnOp (BitNot, e) ->
      let t = type_check_expr env e in
      if types_equal t (TPrim TBool) then TPrim TBool
      else raise (TypeError "Bitwise not requires boolean type")
  
  | EApp (func, args) ->
      let func_type = type_check_expr env func in
      (match func_type with
       | TFunc (param_types, ret_type) ->
           let arg_types = List.map (type_check_expr env) args in
           if List.length param_types = List.length arg_types &&
              List.for_all2 types_equal param_types arg_types then
             ret_type
           else raise (TypeError "Function application type mismatch")
       | _ -> raise (TypeError "Attempting to call non-function"))
  
  | ELambda (params, body) ->
      let param_types = List.map (fun (_, t_opt) ->
        match t_opt with
        | Some t -> t
        | None -> TVar "a"  (* Type inference needed *)
      ) params in
      let new_env = List.fold_left2 (fun e (name, _) t ->
        extend_env e name t
      ) env params param_types in
      let ret_type = type_check_expr new_env body in
      TFunc (param_types, ret_type)
  
  | EIf (cond, then_branch, else_branch) ->
      let cond_type = type_check_expr env cond in
      if not (types_equal cond_type (TPrim TBool)) then
        raise (TypeError "If condition must be boolean");
      let then_type = type_check_expr env then_branch in
      let else_type = type_check_expr env else_branch in
      if types_equal then_type else_type then then_type
      else raise (TypeError "If branches must have same type")
  
  | EList elements ->
      (match elements with
       | [] -> TList (TVar "a")  (* Empty list *)
       | e :: rest ->
           let elem_type = type_check_expr env e in
           List.iter (fun elem ->
             let t = type_check_expr env elem in
             if not (types_equal t elem_type) then
               raise (TypeError "List elements must have same type")
           ) rest;
           TList elem_type)
  
  | ESet elements ->
      (match elements with
       | [] -> TSet (TVar "a")
       | e :: rest ->
           let elem_type = type_check_expr env e in
           List.iter (fun elem ->
             let t = type_check_expr env elem in
             if not (types_equal t elem_type) then
               raise (TypeError "Set elements must have same type")
           ) rest;
           TSet elem_type)
  
  | ETuple elements ->
      let types = List.map (type_check_expr env) elements in
      TTuple types
  
  | EAccess (arr, index) ->
      let arr_type = type_check_expr env arr in
      let index_type = type_check_expr env index in
      if not (types_equal index_type (TPrim TInt)) then
        raise (TypeError "Array index must be integer");
      (match arr_type with
       | TList t | TArray (t, _) -> t
       | _ -> raise (TypeError "Can only index lists and arrays"))
  
  | EField (_struct_expr, _field) ->
      (* TODO: Implement struct field access type checking *)
      raise (TypeError "Struct field access not yet implemented")
  
  | EStruct (name, _fields) ->
      (* TODO: Implement struct construction type checking *)
      TNamed name
  
  | EGuard guards ->
      (* All guard bodies must have the same type *)
      (match guards with
       | [] -> raise (TypeError "Empty guard clause")
       | g :: rest ->
           let body_type = type_check_expr env g.body in
           List.iter (fun guard ->
             let cond_type = type_check_expr env guard.condition in
             if not (types_equal cond_type (TPrim TBool)) then
               raise (TypeError "Guard condition must be boolean");
             let t = type_check_expr env guard.body in
             if not (types_equal t body_type) then
               raise (TypeError "All guard bodies must have same type")
           ) rest;
           body_type)
  
  | ELet (name, type_opt, value, body) ->
      let value_type = type_check_expr env value in
      (match type_opt with
       | Some t ->
           if not (types_equal value_type t) then
             raise (TypeError "Let binding type mismatch")
       | None -> ());
      let new_env = extend_env env name value_type in
      type_check_expr new_env body
  
  | EBlock (stmts, ret_opt) ->
      let final_env = List.fold_left (fun e stmt ->
        match stmt with
        | SLet (name, type_opt, value) ->
            let value_type = type_check_expr e value in
            (match type_opt with
             | Some t ->
                 if not (types_equal value_type t) then
                   raise (TypeError "Statement type mismatch")
             | None -> ());
            extend_env e name value_type
        | SExpr expr ->
            let _ = type_check_expr e expr in
            e
        | SReturn expr ->
            let _ = type_check_expr e expr in
            e
      ) env stmts in
      (match ret_opt with
       | Some ret_expr -> type_check_expr final_env ret_expr
       | None -> TPrim TNil)
  
  | EVon _fields ->
      (* TODO: Check field types if needed *)
      TNamed "Von"
  
  | EOutput e ->
      let _ = type_check_expr env e in
      TPrim TNil
  
  | EMatch (_, _) ->
      (* TODO: Implement pattern matching type checking *)
      raise (TypeError "Pattern matching type checking not yet implemented")

(* Type check a function definition *)
let type_check_func (env: type_env) (func: func_def) : type_env =
  let param_types = List.map (fun p ->
    match p.ptype with
    | Some t -> (p.name, t)
    | None -> raise (TypeError (Printf.sprintf "Parameter %s needs type annotation" p.name))
  ) func.params in
  
  let ret_type = match func.return_type with
    | Some t -> t
    | None -> TPrim TNil
  in
  
  let func_type = TFunc (List.map snd param_types, ret_type) in
  let env_with_func = extend_env env func.fname func_type in
  
  let func_env = List.fold_left (fun e (name, t) ->
    extend_env e name t
  ) env_with_func param_types in
  
  let body_type = type_check_expr func_env func.body in
  
  if not (types_equal body_type ret_type) then
    raise (TypeError (Printf.sprintf "Function %s return type mismatch" func.fname));
  
  env_with_func
