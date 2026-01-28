#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(String),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    StructInit(String, Vec<(String, Expr)>),
    VonInit(Vec<(String, Expr)>),
    Output(Box<Expr>),
    Let(String, Option<Type>, Box<Expr>, Box<Expr>),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    Guards(Vec<(Expr, Expr)>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(String, Option<Type>, Expr),
    Return(Expr),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Nil,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add, Sub, Mul, Div, Exp, FloorDiv,
    Eq, Geq, Leq, Neq, Lt, Gt,
    And, Or,
    BitAnd, BitOr, BitXor,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg, Not, BitNot,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Uint,
    Float,
    Bool,
    Char,
    String,
    Nil,
    Von,
    Named(String),
    List(Box<Type>),
    Array(Box<Type>, usize),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Expr,
    pub is_proc: bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ptype: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ModuleMember {
    Func(FuncDef),
    Data(String, Vec<(String, Type)>),
    SubModule(String, Vec<ModuleMember>),
}

#[derive(Debug, Clone)]
pub struct AppDef {
    pub name: String,
    pub members: Vec<ModuleMember>,
}

#[derive(Debug, Clone)]
pub enum Program {
    App(AppDef),
    Module(String, Vec<ModuleMember>),
}
