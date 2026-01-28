use chumsky::prelude::*;
use crate::frontend::lexer::Token;
use crate::frontend::ast::*;

pub fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let expr = recursive(|expr| {
        let atom = recursive(|atom| {
            let literal = select! {
                Token::Int(s) => Expr::Literal(Literal::Int(s.parse().unwrap())),
                Token::Float(s) => Expr::Literal(Literal::Float(s.parse().unwrap())),
                Token::True => Expr::Literal(Literal::Bool(true)),
                Token::False => Expr::Literal(Literal::Bool(false)),
                Token::Char(c) => Expr::Literal(Literal::Char(c)),
                Token::String(s) => Expr::Literal(Literal::String(s)),
                Token::Nil => Expr::Literal(Literal::Nil),
            };

            let var = select! {
                Token::Identifier(s) => Expr::Var(s),
            };

            let von_init = just(Token::Von)
                .ignore_then(just(Token::LBrace))
                .ignore_then(select! { Token::Identifier(s) => s }
                    .then_ignore(just(Token::Assign))
                    .then(expr.clone())
                    .separated_by(just(Token::Comma)))
                .then_ignore(just(Token::RBrace))
                .map(|fields| Expr::VonInit(fields));

            let output = just(Token::AtOutput)
                .ignore_then(just(Token::LParen))
                .ignore_then(expr.clone())
                .then_ignore(just(Token::RParen))
                .map(|e| Expr::Output(Box::new(e)));

            let guards = just(Token::Pipe)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Arrow))
                .then(expr.clone())
                .repeated()
                .at_least(1)
                .map(|g| Expr::Guards(g));

            literal
                .or(von_init)
                .or(output)
                .or(guards)
                .or(var)
                .or(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))
        });

        let call = atom.clone()
            .then(just(Token::LParen)
                .ignore_then(expr.clone().separated_by(just(Token::Comma)))
                .then_ignore(just(Token::RParen))
                .repeated())
            .foldl(|lhs, args| Expr::Call(Box::new(lhs), args));

        // Expression precedence (simplified)
        let op = just(Token::Plus).to(BinOp::Add)
            .or(just(Token::Minus).to(BinOp::Sub))
            .or(just(Token::Mul).to(BinOp::Mul))
            .or(just(Token::Div).to(BinOp::Div))
            .or(just(Token::Eq).to(BinOp::Eq));
        
        call.clone().then(op.then(call.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)))
            .or(call.clone())
    });

    let enum_type = just(Token::UpperIdentifier("Int".to_string())).to(Type::Int)
        .or(just(Token::UpperIdentifier("Float".to_string())).to(Type::Float))
        .or(just(Token::UpperIdentifier("Bool".to_string())).to(Type::Bool))
        .or(just(Token::UpperIdentifier("String".to_string())).to(Type::String))
        .or(just(Token::UpperIdentifier("Von".to_string())).to(Type::Von));

    let param = select! { Token::Identifier(s) => s }
        .then(just(Token::Colon).ignore_then(enum_type.clone()).or_not())
        .map(|(name, ptype)| Param { name, ptype });

    let func = just(Token::Func)
        .ignore_then(select! { Token::Identifier(s) => s })
        .then(just(Token::LParen).ignore_then(param.separated_by(just(Token::Comma))).then_ignore(just(Token::RParen)))
        .then(just(Token::TypeArrow).ignore_then(enum_type.clone()).or_not())
        .then(just(Token::LBrace).ignore_then(expr.clone().repeated()).then_ignore(just(Token::RBrace)))
        .map(|(((name, params), ret), body)| ModuleMember::Func(FuncDef {
            name,
            params,
            return_type: ret,
            body: Expr::Block(body.into_iter().map(Stmt::Expr).collect(), None), // Simplified body
            is_proc: false,
        }));

    let start = just(Token::AtStart)
        .ignore_then(just(Token::LParen))
        .ignore_then(just(Token::RParen))
        .ignore_then(just(Token::LBrace))
        .ignore_then(expr.clone().repeated()) // Statements as exprs for now
        .then(just(Token::Arrow).ignore_then(expr.clone()).or_not())
        .then_ignore(just(Token::RBrace))
        .map(|(stmts, ret)| ModuleMember::Func(FuncDef {
            name: "main".to_string(),
            params: Vec::new(),
            return_type: Some(Type::Von),
            body: Expr::Block(
                stmts.into_iter().map(Stmt::Expr).collect(),
                ret.map(Box::new)
            ),
            is_proc: false,
        }));

    let app_member = func.or(start);

    let app = just(Token::App)
        .ignore_then(select! { Token::UpperIdentifier(s) => s })
        .then(just(Token::LBrace)
            .ignore_then(app_member.repeated())
            .then_ignore(just(Token::RBrace)))
        .map(|(name, members)| Program::App(AppDef {
            name,
            members,
        }));

    app
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, Vec<Simple<Token>>> {
    parser().parse(tokens)
}
