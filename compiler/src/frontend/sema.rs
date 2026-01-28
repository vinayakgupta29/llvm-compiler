use std::collections::HashMap;
use crate::frontend::ast::*;
use crate::backend::hir::*;

pub struct Sema {
    env: HashMap<String, HIRType>,
}

impl Sema {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        // Prelude: otherwise is True
        env.insert("otherwise".to_string(), HIRType::Bool);
        Sema { env }
    }

    pub fn analyze(&mut self, prog: Program) -> Result<HIRProgram, String> {
        match prog {
            Program::App(app) => {
                let mut members = Vec::new();
                for m in app.members {
                    members.push(self.analyze_member(m)?);
                }
                Ok(HIRProgram::App {
                    name: app.name,
                    imports: Vec::new(),
                    members,
                })
            }
            Program::Module(name, ast_members) => {
                let mut members = Vec::new();
                for m in ast_members {
                    members.push(self.analyze_member(m)?);
                }
                Ok(HIRProgram::Module {
                    name,
                    exports: Vec::new(),
                    members,
                })
            }
        }
    }

    fn analyze_member(&mut self, member: ModuleMember) -> Result<HIRModuleMember, String> {
        match member {
            ModuleMember::Func(f) => {
                let hf = self.analyze_func(f)?;
                Ok(HIRModuleMember::Func { func: hf })
            }
            _ => Err("Member type not yet supported in Sema".to_string()),
        }
    }

    fn analyze_func(&mut self, f: FuncDef) -> Result<HIRFunc, String> {
        let mut params = Vec::new();
        
        for p in &f.params {
            let ty = self.analyze_type(p.ptype.clone().unwrap_or(Type::Int))?;
            params.push(HIRParam {
                name: p.name.clone(),
                param_type: ty,
            });
        }

        let ret_type = self.analyze_type(f.return_type.clone().unwrap_or(Type::Int))?;
        
        // Add function itself to env for recursion
        let func_type = HIRType::Func {
            params: params.iter().map(|p| p.param_type.clone()).collect(),
            return_type: Box::new(ret_type.clone()),
        };
        self.env.insert(f.name.clone(), func_type);

        let mut local_env = self.env.clone();
        for p in &params {
            local_env.insert(p.name.clone(), p.param_type.clone());
        }

        let body = self.analyze_expr(f.body, &local_env)?;

        Ok(HIRFunc {
            name: f.name,
            params,
            return_type: ret_type,
            body,
            is_proc: f.is_proc,
        })
    }

    fn analyze_expr(&self, expr: Expr, env: &HashMap<String, HIRType>) -> Result<HIRExpr, String> {
        match expr {
            Expr::Literal(l) => match l {
                Literal::Int(i) => Ok(HIRExpr::LitInt { value: i }),
                Literal::Float(f) => Ok(HIRExpr::LitFloat { value: f }),
                Literal::Bool(b) => Ok(HIRExpr::LitBool { value: b }),
                Literal::String(s) => Ok(HIRExpr::LitString { value: s }),
                _ => Ok(HIRExpr::LitNil),
            },
            Expr::Var(name) => {
                if name == "otherwise" {
                    return Ok(HIRExpr::LitBool { value: true });
                }
                let ty = env.get(&name).cloned().ok_or_else(|| format!("Undefined variable: {}", name))?;
                Ok(HIRExpr::Var { name, var_type: ty })
            }
            Expr::BinOp(op, left, right) => {
                let hleft = self.analyze_expr(*left, env)?;
                let hright = self.analyze_expr(*right, env)?;
                Ok(HIRExpr::BinOp {
                    op: match op {
                        BinOp::Add => HIRBinOp::Add,
                        BinOp::Sub => HIRBinOp::Sub,
                        BinOp::Mul => HIRBinOp::Mul,
                        BinOp::Div => HIRBinOp::Div,
                        BinOp::Eq => HIRBinOp::Eq,
                        _ => HIRBinOp::Add,
                    },
                    left: Box::new(hleft),
                    right: Box::new(hright),
                    expr_type: HIRType::Int,
                })
            }
            Expr::Call(func, args) => {
                let hfunc = self.analyze_expr(*func, env)?;
                let mut hargs = Vec::new();
                for a in args {
                    hargs.push(self.analyze_expr(a, env)?);
                }
                Ok(HIRExpr::Call {
                    func: Box::new(hfunc),
                    args: hargs,
                    expr_type: HIRType::Int,
                })
            }
            Expr::VonInit(fields) => {
                let mut hfields = Vec::new();
                for (name, val) in fields {
                    hfields.push(FieldInit {
                        name,
                        value: self.analyze_expr(val, env)?,
                    });
                }
                Ok(HIRExpr::VonInit {
                    fields: hfields,
                    expr_type: HIRType::Struct {
                        name: "Von".to_string(),
                        fields: Vec::new(),
                    },
                })
            }
            Expr::Output(e) => {
                let val = self.analyze_expr(*e, env)?;
                Ok(HIRExpr::Output {
                    operand: Box::new(val),
                    expr_type: HIRType::Nil,
                })
            }
            Expr::Block(mut stmts, ret) => {
                let hret = if let Some(e) = ret {
                    self.analyze_expr(*e, env)?
                } else if !stmts.is_empty() {
                    if let Stmt::Expr(e) = stmts.pop().unwrap() {
                        self.analyze_expr(e, env)?
                    } else {
                        HIRExpr::LitNil
                    }
                } else {
                    HIRExpr::LitNil
                };

                let mut res = hret;
                for s in stmts.into_iter().rev() {
                    match s {
                        Stmt::Expr(e) => {
                            let val = self.analyze_expr(e, env)?;
                            res = HIRExpr::Let {
                                name: "_".to_string(),
                                var_type: HIRType::Nil,
                                value: Box::new(val),
                                body: Box::new(res),
                                expr_type: HIRType::Int,
                            };
                        }
                        _ => {}
                    }
                }
                Ok(res)
            }
            Expr::Guards(clauses) => {
                // Lower guards to nested Ifs
                let mut res = HIRExpr::LitNil;
                for (cond, val) in clauses.into_iter().rev() {
                    let hcond = self.analyze_expr(cond, env)?;
                    let hval = self.analyze_expr(val, env)?;
                    res = HIRExpr::If {
                        condition: Box::new(hcond),
                        then_branch: Box::new(hval),
                        else_branch: Box::new(res),
                        expr_type: HIRType::Int,
                    };
                }
                Ok(res)
            }
            _ => Err(format!("Expression {:?} not yet supported in Sema", expr)),
        }
    }

    fn analyze_type(&self, t: Type) -> Result<HIRType, String> {
        match t {
            Type::Int => Ok(HIRType::Int),
            Type::Float => Ok(HIRType::Float),
            Type::Bool => Ok(HIRType::Bool),
            Type::String => Ok(HIRType::String),
            Type::Nil => Ok(HIRType::Nil),
            Type::Von => Ok(HIRType::Struct { name: "Von".to_string(), fields: Vec::new() }),
            _ => Ok(HIRType::Int),
        }
    }
}
