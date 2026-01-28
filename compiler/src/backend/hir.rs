// HIR data structures for Rust backend
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum HIRType {
    #[serde(rename = "Int")]
    Int,
    #[serde(rename = "Uint")]
    Uint,
    #[serde(rename = "Float")]
    Float,
    #[serde(rename = "Bool")]
    Bool,
    #[serde(rename = "Char")]
    Char,
    #[serde(rename = "String")]
    String,
    #[serde(rename = "Nil")]
    Nil,
    #[serde(rename = "Func")]
    Func {
        params: Vec<HIRType>,
        #[serde(rename = "return")]
        return_type: Box<HIRType>,
    },
    #[serde(rename = "Struct")]
    Struct {
        name: String,
        fields: Vec<StructField>,
    },
    #[serde(rename = "Enum")]
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
    },
    #[serde(rename = "List")]
    List {
        elem_type: Box<HIRType>,
    },
    #[serde(rename = "Array")]
    Array {
        elem_type: Box<HIRType>,
        size: usize,
    },
    #[serde(rename = "Tuple")]
    Tuple {
        types: Vec<HIRType>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    #[serde(rename = "type")]
    pub field_type: HIRType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub types: Vec<HIRType>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum HIRBinOp {
    Add, Sub, Mul, Div, Exp, FloorDiv,
    And, Or, Eq, Neq, Lt, Gt, Leq, Geq,
    BitAnd, BitOr, BitXor,
    Cons, Concat,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum HIRUnOp {
    Neg, Not, BitNot,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "expr")]
pub enum HIRExpr {
    #[serde(rename = "LitInt")]
    LitInt {
        value: i64,
    },
    #[serde(rename = "LitFloat")]
    LitFloat {
        value: f64,
    },
    #[serde(rename = "LitBool")]
    LitBool {
        value: bool,
    },
    #[serde(rename = "LitChar")]
    LitChar {
        value: String,
    },
    #[serde(rename = "LitString")]
    LitString {
        value: String,
    },
    #[serde(rename = "LitNil")]
    LitNil,
    #[serde(rename = "Var")]
    Var {
        name: String,
        #[serde(rename = "type")]
        var_type: HIRType,
    },
    #[serde(rename = "BinOp")]
    BinOp {
        op: HIRBinOp,
        left: Box<HIRExpr>,
        right: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "UnOp")]
    UnOp {
        op: HIRUnOp,
        operand: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "Call")]
    Call {
        func: Box<HIRExpr>,
        args: Vec<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "If")]
    If {
        condition: Box<HIRExpr>,
        #[serde(rename = "then")]
        then_branch: Box<HIRExpr>,
        #[serde(rename = "else")]
        else_branch: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "Let")]
    Let {
        name: String,
        var_type: HIRType,
        value: Box<HIRExpr>,
        body: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "List")]
    List {
        elements: Vec<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "Tuple")]
    Tuple {
        elements: Vec<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "Access")]
    Access {
        array: Box<HIRExpr>,
        index: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "StructInit")]
    StructInit {
        name: String,
        fields: Vec<FieldInit>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "VonInit")]
    VonInit {
        fields: Vec<FieldInit>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
    #[serde(rename = "Output")]
    Output {
        operand: Box<HIRExpr>,
        #[serde(rename = "type")]
        expr_type: HIRType,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInit {
    pub name: String,
    pub value: HIRExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HIRParam {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: HIRType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HIRFunc {
    pub name: String,
    pub params: Vec<HIRParam>,
    pub return_type: HIRType,
    pub body: HIRExpr,
    pub is_proc: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum HIRModuleMember {
    #[serde(rename = "Func")]
    Func {
        func: HIRFunc,
    },
    #[serde(rename = "Directive")]
    Directive {
        name: String,
        func: HIRFunc,
    },
    #[serde(rename = "Let")]
    Let {
        name: String,
        var_type: HIRType,
        value: HIRExpr,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum HIRProgram {
    #[serde(rename = "Module")]
    Module {
        name: String,
        exports: Vec<String>,
        members: Vec<HIRModuleMember>,
    },
    #[serde(rename = "App")]
    App {
        name: String,
        imports: Vec<String>,
        members: Vec<HIRModuleMember>,
    },
}
