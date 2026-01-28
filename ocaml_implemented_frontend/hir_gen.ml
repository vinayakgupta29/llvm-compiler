(* HIR generation and serialization *)

open Hir


(* Serialize HIR type to JSON *)
let rec hir_type_to_json (t: hir_type) : Yojson.Basic.t =
  match t with
  | HInt -> `Assoc [("type", `String "Int")]
  | HUint -> `Assoc [("type", `String "Uint")]
  | HFloat -> `Assoc [("type", `String "Float")]
  | HBool -> `Assoc [("type", `String "Bool")]
  | HChar -> `Assoc [("type", `String "Char")]
  | HString -> `Assoc [("type", `String "String")]
  | HNil -> `Assoc [("type", `String "Nil")]
  | HFunc (params, ret) ->
      `Assoc [
        ("type", `String "Func");
        ("params", `List (List.map hir_type_to_json params));
        ("return", hir_type_to_json ret)
      ]
  | HStruct (name, fields) ->
      `Assoc [
        ("type", `String "Struct");
        ("name", `String name);
        ("fields", `List (List.map (fun (fname, ftype) ->
          `Assoc [("name", `String fname); ("type", hir_type_to_json ftype)]
        ) fields))
      ]
  | HEnum (name, variants) ->
      `Assoc [
        ("type", `String "Enum");
        ("name", `String name);
        ("variants", `List (List.map (fun (vname, vtypes) ->
          `Assoc [
            ("name", `String vname);
            ("types", `List (List.map hir_type_to_json vtypes))
          ]
        ) variants))
      ]
  | HList elem_type ->
      `Assoc [
        ("type", `String "List");
        ("elem_type", hir_type_to_json elem_type)
      ]
  | HArray (elem_type, size) ->
      `Assoc [
        ("type", `String "Array");
        ("elem_type", hir_type_to_json elem_type);
        ("size", `Int size)
      ]
  | HTuple types ->
      `Assoc [
        ("type", `String "Tuple");
        ("types", `List (List.map hir_type_to_json types))
      ]

(* Serialize HIR binop to JSON *)
let hir_binop_to_json (op: hir_binop) : Yojson.Basic.t =
  let op_str = match op with
    | HAdd -> "Add" | HSub -> "Sub" | HMul -> "Mul" | HDiv -> "Div"
    | HExp -> "Exp" | HFloorDiv -> "FloorDiv"
    | HAnd -> "And" | HOr -> "Or"
    | HEq -> "Eq" | HNeq -> "Neq" | HLt -> "Lt" | HGt -> "Gt"
    | HLeq -> "Leq" | HGeq -> "Geq"
    | HBitAnd -> "BitAnd" | HBitOr -> "BitOr" | HBitXor -> "BitXor"
    | HCons -> "Cons" | HConcat -> "Concat"
  in
  `String op_str

(* Serialize HIR unop to JSON *)
let hir_unop_to_json (op: hir_unop) : Yojson.Basic.t =
  let op_str = match op with
    | HNeg -> "Neg" | HNot -> "Not" | HBitNot -> "BitNot"
  in
  `String op_str

(* Serialize HIR expression to JSON *)
let rec hir_expr_to_json (expr: hir_expr) : Yojson.Basic.t =
  match expr with
  | HLitInt i -> `Assoc [("expr", `String "LitInt"); ("value", `Int i)]
  | HLitFloat f -> `Assoc [("expr", `String "LitFloat"); ("value", `Float f)]
  | HLitBool b -> `Assoc [("expr", `String "LitBool"); ("value", `Bool b)]
  | HLitChar c -> `Assoc [("expr", `String "LitChar"); ("value", `String (String.make 1 c))]
  | HLitString s -> `Assoc [("expr", `String "LitString"); ("value", `String s)]
  | HLitNil -> `Assoc [("expr", `String "LitNil")]
  
  | HVar (name, t) ->
      `Assoc [
        ("expr", `String "Var");
        ("name", `String name);
        ("type", hir_type_to_json t)
      ]
  
  | HBinOp (op, e1, e2, t) ->
      `Assoc [
        ("expr", `String "BinOp");
        ("op", hir_binop_to_json op);
        ("left", hir_expr_to_json e1);
        ("right", hir_expr_to_json e2);
        ("type", hir_type_to_json t)
      ]
  
  | HUnOp (op, e, t) ->
      `Assoc [
        ("expr", `String "UnOp");
        ("op", hir_unop_to_json op);
        ("operand", hir_expr_to_json e);
        ("type", hir_type_to_json t)
      ]
  
  | HCall (func, args, t) ->
      `Assoc [
        ("expr", `String "Call");
        ("func", hir_expr_to_json func);
        ("args", `List (List.map hir_expr_to_json args));
        ("type", hir_type_to_json t)
      ]
  
  | HIf (cond, then_br, else_br, t) ->
      `Assoc [
        ("expr", `String "If");
        ("condition", hir_expr_to_json cond);
        ("then", hir_expr_to_json then_br);
        ("else", hir_expr_to_json else_br);
        ("type", hir_type_to_json t)
      ]
  
  | HLet (name, t, value, body, ret_t) ->
      `Assoc [
        ("expr", `String "Let");
        ("name", `String name);
        ("var_type", hir_type_to_json t);
        ("value", hir_expr_to_json value);
        ("body", hir_expr_to_json body);
        ("type", hir_type_to_json ret_t)
      ]
  
  | HList (elements, t) ->
      `Assoc [
        ("expr", `String "List");
        ("elements", `List (List.map hir_expr_to_json elements));
        ("type", hir_type_to_json t)
      ]
  
  | HTupleExpr (elements, t) ->
      `Assoc [
        ("expr", `String "Tuple");
        ("elements", `List (List.map hir_expr_to_json elements));
        ("type", hir_type_to_json t)
      ]
  
  | HAccess (arr, idx, t) ->
      `Assoc [
        ("expr", `String "Access");
        ("array", hir_expr_to_json arr);
        ("index", hir_expr_to_json idx);
        ("type", hir_type_to_json t)
      ]
  
  | HStructInit (name, fields, t) ->
      `Assoc [
        ("expr", `String "StructInit");
        ("name", `String name);
        ("fields", `List (List.map (fun (fname, fexpr) ->
          `Assoc [("name", `String fname); ("value", hir_expr_to_json fexpr)]
        ) fields));
        ("type", hir_type_to_json t)
      ]
  | HVon (fields, t) ->
      `Assoc [
        ("expr", `String "VonInit");
        ("fields", `List (List.map (fun (fname, fexpr) ->
          `Assoc [("name", `String fname); ("value", hir_expr_to_json fexpr)]
        ) fields));
        ("type", hir_type_to_json t)
      ]
  | HOutput (e, t) ->
      `Assoc [
        ("expr", `String "Output");
        ("operand", hir_expr_to_json e);
        ("type", hir_type_to_json t)
      ]

(* Serialize HIR function to JSON *)
let hir_func_to_json (func: hir_func) : Yojson.Basic.t =
  `Assoc [
    ("name", `String func.hir_fname);
    ("params", `List (List.map (fun (pname, ptype) ->
      `Assoc [("name", `String pname); ("type", hir_type_to_json ptype)]
    ) func.hir_params));
    ("return_type", hir_type_to_json func.hir_return_type);
    ("body", hir_expr_to_json func.hir_body);
    ("is_proc", `Bool func.hir_is_proc)
  ]

(* Serialize HIR program to JSON *)
let hir_program_to_json (prog: hir_program) : Yojson.Basic.t =
  match prog with
  | HIRModule mod_def ->
      `Assoc [
        ("kind", `String "Module");
        ("name", `String mod_def.hir_mod_name);
        ("exports", `List (List.map (fun s -> `String s) mod_def.hir_exports));
        ("members", `List (List.map (fun m ->
          match m with
          | HIRFunc f -> `Assoc [("type", `String "Func"); ("func", hir_func_to_json f)]
          | HIRTypeDef _ -> `Assoc [("type", `String "TypeDef")]
          | HIRDirective (s, f) -> `Assoc [("type", `String "Directive"); ("name", `String s); ("func", hir_func_to_json f)]
          | HIRLet (name, t, e) ->
              `Assoc [
                ("type", `String "Let");
                ("name", `String name);
                ("var_type", hir_type_to_json t);
                ("value", hir_expr_to_json e)
              ]
        ) mod_def.hir_members))
      ]
  | HIRApp app_def ->
      `Assoc [
        ("kind", `String "App");
        ("name", `String app_def.hir_app_name);
        ("imports", `List (List.map (fun s -> `String s) app_def.hir_imports));
        ("members", `List (List.map (fun m ->
          match m with
          | HIRFunc f -> `Assoc [("type", `String "Func"); ("func", hir_func_to_json f)]
          | HIRTypeDef _ -> `Assoc [("type", `String "TypeDef")]
          | HIRDirective (s, f) -> `Assoc [("type", `String "Directive"); ("name", `String s); ("func", hir_func_to_json f)]
          | HIRLet (name, t, e) ->
              `Assoc [
                ("type", `String "Let");
                ("name", `String name);
                ("var_type", hir_type_to_json t);
                ("value", hir_expr_to_json e)
              ]
        ) app_def.hir_app_members))
      ]

(* Write HIR to file *)
let write_hir_to_file (filename: string) (prog: hir_program) : unit =
  let json = hir_program_to_json prog in
  let oc = open_out filename in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc
