// Textual LLVM IR code generation
use std::collections::HashMap;
use std::fmt::Write;

use crate::backend::hir::*;

pub struct CodeGen {
    output: String,
    variables: HashMap<String, (String, String)>, // var name -> (llvm register, llvm type)
    von_fields: Vec<(String, String, String)>,    // (key, reg, type)
    temp_count: usize,
    global_strings: Vec<(String, String)>, // (label, content)
    output_filename_reg: Option<String>,
}

impl CodeGen {
    pub fn new() -> Self {
        CodeGen {
            output: String::new(),
            variables: HashMap::new(),
            von_fields: Vec::new(),
            temp_count: 0,
            global_strings: Vec::new(),
            output_filename_reg: None,
        }
    }
    
    fn next_temp(&mut self) -> String {
        self.temp_count += 1;
        format!("%t{}", self.temp_count)
    }

    fn next_global(&mut self) -> String {
        format!("@.str.{}", self.global_strings.len())
    }

    fn hir_type_to_llvm_str(&self, hir_type: &HIRType) -> String {
        match hir_type {
            HIRType::Int => "i64".to_string(),
            HIRType::Uint => "i64".to_string(),
            HIRType::Float => "double".to_string(),
            HIRType::Bool => "i1".to_string(),
            HIRType::Char => "i8".to_string(),
            HIRType::String => "i8*".to_string(),
            HIRType::Nil => "void".to_string(),
            HIRType::List { .. } => "i8*".to_string(),
            HIRType::Array { elem_type, size } => format!("[{} x {}]", size, self.hir_type_to_llvm_str(elem_type)),
            HIRType::Tuple { types } => {
                let llvm_types: Vec<String> = types.iter()
                    .map(|t| self.hir_type_to_llvm_str(t))
                    .collect();
                format!("{{ {} }}", llvm_types.join(", "))
            },
            _ => "i64".to_string(),
        }
    }
    
    // Generate code for HIR expression
    fn gen_expr(&mut self, expr: &HIRExpr) -> Result<String, String> {
        match expr {
            HIRExpr::LitInt { value } => Ok(format!("{}", value)),
            HIRExpr::LitFloat { value } => Ok(format!("{:e}", value)),
            HIRExpr::LitBool { value } => Ok(if *value { "1".to_string() } else { "0".to_string() }),
            HIRExpr::LitString { value } => {
                let label = self.next_global();
                self.global_strings.push((label.clone(), value.clone()));
                let res = self.next_temp();
                writeln!(self.output, "  {} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i64 0, i64 0", 
                    res, value.len() + 1, value.len() + 1, label).unwrap();
                Ok(res)
            },
            HIRExpr::LitNil => Ok("0".to_string()),
            HIRExpr::Var { name, .. } => {
                if let Some((reg, _ty)) = self.variables.get(name) {
                    Ok(reg.clone())
                } else {
                    // Fallback for global function names
                    Ok(name.clone())
                }
            },
            HIRExpr::BinOp { op, left, right, expr_type } => {
                let lhs = self.gen_expr(left)?;
                let rhs = self.gen_expr(right)?;
                let res = self.next_temp();
                let ty = self.hir_type_to_llvm_str(expr_type);
                
                let op_str = match op {
                    HIRBinOp::Add => if ty == "double" { "fadd" } else { "add" },
                    HIRBinOp::Sub => if ty == "double" { "fsub" } else { "sub" },
                    HIRBinOp::Mul => if ty == "double" { "fmul" } else { "mul" },
                    HIRBinOp::Div => match expr_type {
                        HIRType::Float => "fdiv",
                        HIRType::Uint => "udiv",
                        _ => "sdiv",
                    },
                    HIRBinOp::Lt => match expr_type {
                        HIRType::Float => "fcmp olt",
                        HIRType::Uint => "icmp ult",
                        _ => "icmp slt",
                    },
                    HIRBinOp::Eq => if ty == "double" { "fcmp oeq" } else { "icmp eq" },
                    _ => unimplemented!("Op {:?} not implemented", op),
                };
                
                writeln!(self.output, "  {} = {} {} {}, {}", res, op_str, ty, lhs, rhs).unwrap();
                Ok(res)
            },
            HIRExpr::If { condition, then_branch, else_branch, expr_type } => {
                let cond = self.gen_expr(condition)?;
                let res_ptr = self.next_temp();
                let ty = self.hir_type_to_llvm_str(expr_type);
                writeln!(self.output, "  {} = alloca {}", res_ptr, ty).unwrap();
                
                let label_then = self.temp_count + 1;
                let label_else = self.temp_count + 2;
                let label_cont = self.temp_count + 3;
                self.temp_count += 3;
                
                writeln!(self.output, "  br i1 {}, label %L{}, label %L{}", cond, label_then, label_else).unwrap();
                
                // Then
                writeln!(self.output, "L{}:", label_then).unwrap();
                let then_res = self.gen_expr(then_branch)?;
                writeln!(self.output, "  store {} {}, {}* {}", ty, then_res, ty, res_ptr).unwrap();
                writeln!(self.output, "  br label %L{}", label_cont).unwrap();
                
                // Else
                writeln!(self.output, "L{}:", label_else).unwrap();
                let else_res = self.gen_expr(else_branch)?;
                writeln!(self.output, "  store {} {}, {}* {}", ty, else_res, ty, res_ptr).unwrap();
                writeln!(self.output, "  br label %L{}", label_cont).unwrap();
                
                // Cont
                writeln!(self.output, "L{}:", label_cont).unwrap();
                let res = self.next_temp();
                writeln!(self.output, "  {} = load {}, {}* {}", res, ty, ty, res_ptr).unwrap();
                
                Ok(res)
            },
            HIRExpr::Call { func, args, expr_type } => {
                let f_res = self.gen_expr(func)?;
                let mut arg_res = Vec::new();
                for a in args {
                    arg_res.push(self.gen_expr(a)?);
                }
                let res = self.next_temp();
                let ty = self.hir_type_to_llvm_str(expr_type);
                
                let arg_strs: Vec<String> = arg_res.iter().map(|r| format!("i64 {}", r)).collect();
                writeln!(self.output, "  {} = call {} @{}({})", res, ty, f_res, arg_strs.join(", ")).unwrap();
                Ok(res)
            },
            HIRExpr::VonInit { fields, expr_type } => {
                let res = self.next_temp();
                let _ty = self.hir_type_to_llvm_str(expr_type);
                
                self.von_fields.clear();
                for f in fields {
                    let val_reg = self.gen_expr(&f.value)?;
                    let v_ty = match &f.value {
                        HIRExpr::LitString { .. } => "i8*",
                        _ => "i64",
                    };
                    self.von_fields.push((f.name.clone(), val_reg, v_ty.to_string()));
                }
                
                writeln!(self.output, "  {} = add i64 0, 0 ; Von literal", res).unwrap();
                Ok(res)
            },
            HIRExpr::Output { operand, .. } => {
                let val = self.gen_expr(operand)?;
                self.output_filename_reg = Some(val.clone());
                Ok(val)
            },
            HIRExpr::Let { name, var_type: _, value, body, expr_type: _ } => {
                let val = self.gen_expr(value)?;
                if name != "_" {
                    // We need the type too... hack: use i64
                    self.variables.insert(name.clone(), (val, "i64".to_string()));
                }
                self.gen_expr(body)
            },
            _ => Err(format!("Expression {:?} not yet implemented in textual codegen", expr)),
        }
    }
    
    pub fn gen_function(&mut self, func: &HIRFunc) -> Result<(), String> {
        let mut ret_ty = self.hir_type_to_llvm_str(&func.return_type);
        let params: Vec<String> = func.params.iter()
            .map(|p| format!("{} %{}", self.hir_type_to_llvm_str(&p.param_type), p.name))
            .collect();
            
        // Special case for main return type (always i64 for consistency)
        if func.name == "main" {
            ret_ty = "i64".to_string();
        }

        writeln!(self.output, "define {} @{}({}) {{", ret_ty, func.name, params.join(", ")).unwrap();
        writeln!(self.output, "entry:").unwrap();
        
        for param in &func.params {
            self.variables.insert(param.name.clone(), (format!("%{}", param.name), self.hir_type_to_llvm_str(&param.param_type)));
        }
        
        let body_res = self.gen_expr(&func.body)?;
        
        if func.name == "main" {
            if let Some(filename_reg) = self.output_filename_reg.clone() {
                let fp = self.next_temp();
                let mode = self.next_global();
                self.global_strings.push((mode.clone(), "w".to_string()));
                let mode_ptr = self.next_temp();
                writeln!(self.output, "  {} = getelementptr inbounds [2 x i8], [2 x i8]* {}, i64 0, i64 0", mode_ptr, mode).unwrap();
                writeln!(self.output, "  {} = call i8* @fopen(i8* {}, i8* {})", fp, filename_reg, mode_ptr).unwrap();
                
                let header = self.next_global();
                self.global_strings.push((header.clone(), "{ ".to_string()));
                let header_ptr = self.next_temp();
                writeln!(self.output, "  {} = getelementptr inbounds [3 x i8], [3 x i8]* {}, i64 0, i64 0", header_ptr, header).unwrap();
                writeln!(self.output, "  call i32 @fputs(i8* {}, i8* {})", header_ptr, fp).unwrap();

                for (i, (key, val_reg, v_ty)) in self.von_fields.clone().into_iter().enumerate() {
                    if i > 0 {
                        let comma = self.next_global();
                        self.global_strings.push((comma.clone(), ", ".to_string()));
                        let comma_ptr = self.next_temp();
                        writeln!(self.output, "  {} = getelementptr inbounds [3 x i8], [3 x i8]* {}, i64 0, i64 0", comma_ptr, comma).unwrap();
                        writeln!(self.output, "  call i32 @fputs(i8* {}, i8* {})", comma_ptr, fp).unwrap();
                    }

                    let k_str = format!("{} = ", key);
                    let k_label = self.next_global();
                    self.global_strings.push((k_label.clone(), k_str.clone()));
                    let k_ptr = self.next_temp();
                    writeln!(self.output, "  {} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i64 0, i64 0", k_ptr, k_str.len() + 1, k_str.len() + 1, k_label).unwrap();
                    writeln!(self.output, "  call i32 @fputs(i8* {}, i8* {})", k_ptr, fp).unwrap();

                    let buf = self.next_temp();
                    writeln!(self.output, "  {} = alloca [64 x i8]", buf).unwrap();
                    let buf_ptr = self.next_temp();
                    writeln!(self.output, "  {} = getelementptr inbounds [64 x i8], [64 x i8]* {}, i64 0, i64 0", buf_ptr, buf).unwrap();
                    
                    let fmt_str = if v_ty == "i8*" { "\"%s\"" } else { "%ld" };
                    let fmt = self.next_global();
                    self.global_strings.push((fmt.clone(), fmt_str.to_string()));
                    let fmt_ptr = self.next_temp();
                    writeln!(self.output, "  {} = getelementptr inbounds [{} x i8], [{} x i8]* {}, i64 0, i64 0", fmt_ptr, fmt_str.len() + 1, fmt_str.len() + 1, fmt).unwrap();
                    
                    writeln!(self.output, "  call i32 (i8*, i8*, ...) @sprintf(i8* {}, i8* {}, {} {})", buf_ptr, fmt_ptr, v_ty, val_reg).unwrap();
                    writeln!(self.output, "  call i32 @fputs(i8* {}, i8* {})", buf_ptr, fp).unwrap();
                }

                let footer = self.next_global();
                self.global_strings.push((footer.clone(), " }".to_string()));
                let footer_ptr = self.next_temp();
                writeln!(self.output, "  {} = getelementptr inbounds [3 x i8], [3 x i8]* {}, i64 0, i64 0", footer_ptr, footer).unwrap();
                writeln!(self.output, "  call i32 @fputs(i8* {}, i8* {})", footer_ptr, fp).unwrap();
                writeln!(self.output, "  call i32 @fclose(i8* {})", fp).unwrap();
            }
            writeln!(self.output, "  ret i64 0").unwrap();
        } else if func.return_type == HIRType::Nil {
            writeln!(self.output, "  ret void").unwrap();
        } else {
            writeln!(self.output, "  ret {} {}", ret_ty, body_res).unwrap();
        }
        
        writeln!(self.output, "}}").unwrap();
        Ok(())
    }
    
    pub fn get_output(&mut self) -> String {
        let final_output = self.output.clone();
        
        // Prepends target triple and global strings
        let mut globals = "target triple = \"x86_64-pc-linux-gnu\"\n\n".to_string();
        for (label, content) in &self.global_strings {
            let len = content.len() + 1;
            // Escape for LLVM: replace \n with \0A, etc.
            let escaped = content.replace("\"", "\\22").replace("\n", "\\0A");
            writeln!(globals, "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"", label, len, escaped).unwrap();
        }
        
        // External declarations
        writeln!(globals, "declare i8* @fopen(i8*, i8*)").unwrap();
        writeln!(globals, "declare i32 @fputs(i8*, i8*)").unwrap();
        writeln!(globals, "declare i32 @sprintf(i8*, i8*, ...)").unwrap();
        writeln!(globals, "declare i32 @fclose(i8*)").unwrap();
        
        format!("{}\n{}", globals, final_output)
    }
}
