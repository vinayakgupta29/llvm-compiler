use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//.*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    // Keywords
    #[token("app", priority = 10)]
    App,
    #[token("mod", priority = 10)]
    Mod,
    #[token("use", priority = 10)]
    Use,
    #[token("func", priority = 10)]
    Func,
    #[token("proc", priority = 10)]
    Proc,
    #[token("data", priority = 10)]
    Data,
    #[token("Von", priority = 10)]
    Von,
    #[token("if", priority = 10)]
    If,
    #[token("else", priority = 10)]
    Else,
    #[token("super", priority = 10)]
    Super,
    #[token("class", priority = 10)]
    Class,
    #[token("instance", priority = 10)]
    Instance,
    #[token("nil", priority = 10)]
    Nil,

    // Directives
    #[token("@start", priority = 10)]
    AtStart,
    #[token("@output", priority = 10)]
    AtOutput,
    #[token("@using", priority = 10)]
    AtUsing,
    #[token("@root", priority = 10)]
    AtRoot,

    // Literals
    #[regex("[a-z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("[A-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    UpperIdentifier(String),
    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    Int(String),
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().to_string())]
    Float(String),
    #[token("true", priority = 10)]
    #[token("t", priority = 10)]
    True,
    #[token("false", priority = 10)]
    #[token("f", priority = 10)]
    False,
    #[regex("'([^'\\\\]|\\\\.)'", |lex| lex.slice().chars().nth(1))]
    Char(char),
    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("^")]
    Exp,
    #[token("_/_")]
    FloorDiv,
    #[token("==")]
    Eq,
    #[token(">=")]
    Geq,
    #[token("<=")]
    Leq,
    #[token("/=")]
    Neq,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token(".&")]
    BitAnd,
    #[token(".|")]
    BitOr,
    #[token(".^")]
    BitXor,
    #[token(".~")]
    BitNot,
    #[token(".")]
    Dot,
    #[token("=>")]
    Arrow,
    #[token("->")]
    TypeArrow,

    // Symbols
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("=")]
    Assign,
    #[token("|")]
    Pipe,
    #[token("@")]
    At,
}
