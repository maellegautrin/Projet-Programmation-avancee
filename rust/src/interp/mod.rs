use self::{
    env::{Env, GEnv, LEnv},
    value::{Value, Function},
};
use crate::parser::ast::*;
use std::{rc::Rc, collections::HashMap};
use crate::parser::ast::Stat_;
mod env;
pub mod value;

impl Block {
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        let nloc=env.locals.extend(&self.locals,Vec::new().into_iter());
        let nenv= &mut Env {locals:nloc , globals: env.globals};
        self.body.interp(nenv);
        self.ret.interp(nenv)
    }
}

impl Stat_ {
    // Interprétation d'une instruction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> () {
        match self {
            Stat_::Nop => (),
            Stat_::Seq(s1, s2) => {
                s1.interp(env);
                s2.interp(env);
                },
            Stat_::StatFunctionCall(fc) => {
                let _ = fc.interp(env);
            },
            Stat_::Assign(v, e) => {
                match v {
                    Var::Name(n) => {
                        let v=e.interp(env);
                        env.set(n, v);
                    },
                    Var::IndexTable(e1, e2) => {
                        let t = e1.interp(env).as_table();
                        let v = Value::as_table_key(e2.interp(env));
                        let val=e.interp(env);
                        t.borrow_mut().insert(v,val);
                    },
                }
            },
            Stat_::If(e, s1, s2) => {
                if e.interp(env).as_bool() {
                    let _ = s1.interp(env);
                } else {
                    let _ = s2.interp(env);
                }
            },
            Stat_::WhileDoEnd(e, s) => {
                while e.interp(env).as_bool() {
                    s.interp(env);
                }
            },
        }
    }
}

impl FunctionCall {
    // Interprétation d'un appel de fonction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        let f1 = self.0.interp(env).as_function();
        match f1 {
            Function::Print => {
            let s_liste = self.1
            .iter()
            .map(|x| x.interp(env).to_string())
            .collect::<Vec<String>>();
                println!("{}", s_liste.join("\t"));
                Value::Nil
            }
            Function::Closure(nl, env2, b) => {
                let s_liste = self.1
                .iter()
                .map(|x| x.interp(env))
                .collect::<Vec<Value<'ast>>>();
                let nloc=env2.extend(nl,s_liste.into_iter());
                let nenv= &mut Env {locals:nloc , globals: env.globals};
                return b.interp(nenv)
        }
    }
}
}

impl Exp_ {
    // Interprétation d'une expression
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        match self {
            Exp_::Number(n) => Value::Number(*n),
            Exp_::LiteralString(s) => Value::String(s.clone()),
            Exp_::True => Value::Bool(true),
            Exp_::False => Value::Bool(false),
            Exp_::Nil => Value::Nil,
            Exp_::Var(v) => {
                match v {
                    Var::Name(n) => env.lookup(n),
                    Var::IndexTable(e1, e2) => {
                        let ve1 = e1.interp(env);
                        match ve1 {
                            Value::Table(t) => {
                                let key = Value::as_table_key(e2.interp(env));
                                match t.borrow().get(&key) {
                                    Some(val) => val.clone(),
                                    None => Value::Nil, 
                                }
                            }
                            _ => Value::Nil,
                        }
                    }
                }
            }
            Exp_::UnOp(op, e) => {
                match op {
                    UnOp::Not => {
                        if e.interp(env).as_bool() {
                            Value::Bool(false)
                        } else {
                            Value::Bool(true)
                        }
                    }
                    UnOp::UnaryMinus => Value::neg(e.interp(env)),
                }
            }
            Exp_::BinOp(op, e1, e2) => {
                match op {
                    BinOp::Equality => {
                        if e1.interp(env) == e2.interp(env) {
                            Value::Bool(true)
                        } else {
                            Value::Bool(false)
                        }
                    }
                    BinOp::Addition => Value::add(e1.interp(env), e2.interp(env)),
                    BinOp::Subtraction => Value::sub(e1.interp(env), e2.interp(env)),
                    BinOp::Multiplication => Value::mul(e1.interp(env), e2.interp(env)),
                    BinOp::Inequality => {
                        if e1.interp(env) == e2.interp(env) {
                            Value::Bool(false)
                        } else {
                            Value::Bool(true)
                        }
                    }
                    BinOp::Less => Value::Bool(e1.interp(env).lt(e2.interp(env))),
                    BinOp::Greater => Value::Bool(e2.interp(env).lt(e1.interp(env))),
                    BinOp::LessEq => Value::Bool(e1.interp(env).le(e2.interp(env))),
                    BinOp::GreaterEq => Value::Bool(e2.interp(env).le(e1.interp(env))),
                    BinOp::LogicalAnd => {
                        let v = e1.interp(env);
                        if v == Value::Nil {
                            Value::Nil
                        } else {
                            if v.as_bool() {
                                e2.interp(env)
                            } else {
                                Value::Bool(false)
                            }
                        }
                    }
                    BinOp::LogicalOr => {
                        let v = e1.interp(env);
                        if v == Value::Nil || v == Value::Bool(false) {
                            e2.interp(env)
                        } else {
                            v
                        }
                    }
                }
            }
            Exp_::ExpFunctionCall(f) => f.interp(env),
            Exp_::FunctionDef(fb) => Value::Function(Function::Closure(&fb.0, env.locals.clone(), &fb.1)),
            Exp_::Table(l) => {
                let mut names = vec![];
                let mut values = vec![];
                for (t1, t2) in l {
                    let t1v = t1.interp(env).as_table_key();
                    let t2v = t2.interp(env);
                    names.push(t1v);
                    values.push(t2v);
                }
                let mut h = HashMap::new();
                for (name, value) in names.into_iter().zip(values.into_iter()) {
                    h.insert(name, value);
                }
                let r=std::cell::RefCell::new(h);
                let rc= Rc::new(r);
                Value::Table(rc)
            }
        }
    }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block) {
    let mut globals = GEnv(HashMap::new());
    let printid = "print".to_owned();
    globals.0.insert(&printid, Value::Function(Function::Print));
    let mut env = Env {
        locals: Rc::new(LEnv::Nil),
        globals: &mut globals,
    };
    ast.interp(&mut env);
}
