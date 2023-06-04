open Luaparser.Ast
type value = Value.t
type env = Value.env

(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  let h= Hashtbl.create 10 in 
  let rec remplissage names values= match names,values with
    |([],_)->()
    |t1::q1,t2::q2-> Hashtbl.add h t1 t2;remplissage q1 q2
    |t1::q1,[]-> Hashtbl.add h t1 (Nil:value); remplissage q1 []
  in 
  remplissage names values; h
(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)

(* Interprète un bloc de code *)
let rec interp_block (env : env) (blk : block) : value =
  let nenv = {env with locals= (create_scope blk.locals [])::env.locals} in
    interp_stat nenv blk.body;
    interp_exp nenv blk.ret
(* Interprète un statement *)
and interp_stat (env : env) (stat : stat) : unit = match stat with
| Nop -> ()
| Seq(s1,s2) -> interp_stat env s1; interp_stat env s2
| FunctionCall(fc)-> let _ =interp_funcall env fc in ()
| Assign(v,e)-> (match v with
                  |Name(n)-> Value.set_ident env n (interp_exp env e)
                  |IndexTable(e1,e2)-> let t= Value.as_table (interp_exp env e1) in 
                                       let v= Value.as_table_key (interp_exp env e2) in 
                                       (Hashtbl.add t v (interp_exp env e))
                                    

)
| If(e,s1,s2)-> (if Value.as_bool(interp_exp env e) then let _=interp_stat env s1 in () 
                else let _= interp_stat env s2 in ())
| WhileDoEnd(e,s)-> while Value.as_bool(interp_exp env e) do 
                     interp_stat env s;
                    done;


(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) : value =
  let (f,arg)=fc in 
  let f1= Value.as_function(interp_exp env f) in 
          
  match f1 with
  | Print -> let s_liste = List.map (fun x-> Value.to_string (interp_exp env x)) arg in print_endline (String.concat ("\t") s_liste); Nil
  | Closure(nl,nenv,b)-> let h=create_scope nl (List.map (fun x->interp_exp env x) arg) in 
                        interp_block {locals= h::nenv.locals; globals=env.globals} b

(* Interprète une expression *)
and interp_exp (env : env) (e : exp) : value = match e with
  | Float(f)-> Float(f)
  | Integer(i)-> Int(i)
  | LiteralString(s)-> String(s)
  | True -> Bool (true)
  | False -> Bool(false)
  | Nil -> Nil
  | Var(v) -> (match v with
              |Name(n)->Value.lookup_ident env n
              |IndexTable(e1,e2)-> let ve1= interp_exp env e1 in (match ve1 with
                                                                    |Table(t)-> (try Hashtbl.find t (Value.as_table_key (interp_exp env e2)) with Not_found -> Nil)
                                                                    |_-> failwith""))
  | UnOp(op,e)-> (match op with           
                           
                  | Not ->if Value.as_bool (interp_exp env e) then Bool(false)
                  else Bool(true)
                  | UnaryMinus -> Value.neg (interp_exp env e)
                  )
  | BinOp(op,e1,e2)->(match op with
                  |Equality-> Bool(Value.equal (interp_exp env e1)  (interp_exp env e2))
                  | Addition -> Value.add (interp_exp env e1) (interp_exp env e2)     
                  | Subtraction-> Value.sub (interp_exp env e1) (interp_exp env e2)
                  | Multiplication -> Value.mul (interp_exp env e1) (interp_exp env e2)
                  | Inequality -> if interp_exp env e1 = interp_exp env e2 then Bool(false)
                                  else Bool(true) 
                  | Less -> Bool(Value.lt (interp_exp env e1) (interp_exp env e2))
                  | Greater -> interp_exp env (UnOp(Not, (BinOp(LessEq,e1,e2))))
                  | LessEq -> Bool(Value.le (interp_exp env e1) (interp_exp env e2))
                  | GreaterEq -> interp_exp env (UnOp(Not,((BinOp(Less,e1,e2)) )))
                  | LogicalAnd -> (let v= (interp_exp env e1) in 
                                    if v= Nil then Nil 
                                    else (if Value.as_bool v then interp_exp env e2 
                                  else Bool(false))
                                    ) 
                  | LogicalOr-> (let v= (interp_exp env e1) in 
                  if (v= Nil || v=Bool(false)) then 
                    (let v2= interp_exp env e2 in 
                     v2)
                  else v 
                  ))
  | FunctionCallE(f)-> interp_funcall env f
  | FunctionDef(e,b)-> Function(Closure(e,env,b))
  | Table(l)-> (let n,v= ref [],ref [] in 
                let rec aux l= match l with
                  |[]->()
                  |(t1,t2)::q-> let t1v=Value.as_table_key(interp_exp env t1) in 
                                let t2v=interp_exp env t2 in 
                                n:=t1v::(!n);
                                v:=t2v::(!v);
                                aux q
                in 
                aux l;
                let h= Hashtbl.create 10 in 
                let rec remplissage names values= match names,values with
                 |([],_)->()
                 |t1::q1,t2::q2-> Hashtbl.add h t1 t2;remplissage q1 q2
                 |t1::q1,[]-> Hashtbl.add h t1 (Nil:value); remplissage q1 []
                 in 
               remplissage (!n) (!v); Table(h)
                                
                  )

(*                         
                                    *)
let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
 (* Format.printf"%s" (show_block ast);*)
  ignore (interp_block env ast)
