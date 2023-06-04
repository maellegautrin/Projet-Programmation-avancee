open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
    let h= Hashtbl.create 10 in 
    let rec remplissage names values= match names,values with
      |([],_)->()
      |t1::q1,t2::q2-> Hashtbl.add h t1 t2;remplissage q1 q2
      |t1::q1,[]-> Hashtbl.add h t1 (Nil:value); remplissage q1 []
    in 
    remplissage names values; h


let rec interp_block (env : env) (co : coroutine) (blk : block) (k : value -> unit) : unit =
  let ht = create_scope blk.locals [] in
  let nenv = {env with locals = ht :: env.locals} in
  interp_stat nenv co blk.body (fun _ -> interp_exp nenv co blk.ret k)
and interp_exp (env : env) (co: coroutine) (e : exp) (k: value-> unit) : unit = match e with
  | Float(f)-> k (Value.Float(f))
  | Integer(i)-> k (Value.Int(i))
  | LiteralString(s)-> k (Value.String(s))
  | True -> k (Value.Bool (true))
  | False -> k (Value.Bool(false))
  | Nil -> k Value.Nil
  | Var v -> (match v with
              | Name n -> k (Value.lookup_ident env n)
              | IndexTable (e1, e2) -> interp_exp env co e1 (fun v1 ->
                                       interp_exp env co e2 (fun v2 ->
                                            let t1 = Value.as_table v1 and k2 = Value.as_table_key v2 in
                                                 if Hashtbl.mem t1 k2 then k (Hashtbl.find t1 k2)
                                                 else k Value.Nil)))                      
  | UnOp(op,e)-> interp_exp env co e (fun v-> match op with
                  | Not ->if Value.as_bool v then k (Bool(false))
                          else k (Bool(true))
                  | UnaryMinus -> k (Value.neg v)
                  )
  | BinOp(op,e1,e2)-> (match op with 
                  |Equality->  interp_exp env co e1 ( fun v1 -> 
                              interp_exp env co e2 (fun v2 -> k (Bool(Value.equal v1 v2))))
                  | Addition -> interp_exp env co e1 ( fun v1 -> 
                                 interp_exp env co e2 (fun v2 -> k (Value.add v1 v2)))   
                  | Subtraction-> interp_exp env co e1 ( fun v1 -> 
                    interp_exp env co e2 (fun v2 -> k (Value.sub v1 v2)))
                  | Multiplication -> interp_exp env co e1 ( fun v1 -> 
                    interp_exp env co e2 (fun v2 -> k (Value.mul v1 v2)))
                  | Inequality ->  interp_exp env co e1 ( fun v1 -> 
                      interp_exp env co e2 (fun v2 -> k (Bool( not (Value.equal v1 v2)))))
                  | Less -> interp_exp env co e1 ( fun v1 -> 
                    interp_exp env co e2 (fun v2 -> k (Bool(Value.lt v1 v2))))
                  | Greater -> interp_exp env co (UnOp(Not, (BinOp(LessEq,e1,e2)))) k
                  | LessEq -> interp_exp env co e1 ( fun v1 -> 
                    interp_exp env co e2
                    (fun v2 -> k (Bool(Value.le v1 v2))))
                  | GreaterEq -> interp_exp env co (UnOp(Not,((BinOp(Less,e1,e2)) ))) k 
                  | LogicalAnd -> (interp_exp env co e1 (fun v1-> 
                                  if not (Value.as_bool v1) then k v1
                                  else interp_exp env co e2 (fun v2 -> k v2)
                                    ) )
                  | LogicalOr-> interp_exp env co e1 (fun v1-> 
                                   if Value.as_bool v1 then k v1
                                  else interp_exp env co e2 (fun v2 -> k v2) 
                                  ) )
  | FunctionCallE(f)-> interp_funcall env co f k
  | FunctionDef(e,b)-> k  (Value.Function(Value.Closure(e,env,b)))
  | Table(l)-> k (let n,v= ref [],ref [] in 
                let rec aux l= match l with
                  |[]->()
                  |(t1,t2)::q-> interp_exp env co t1 (fun v1 -> 
                                let kv1= Value.as_table_key v1 in 
                                interp_exp env co t2 (fun v2 -> 
                                n:=kv1::(!n);
                                v:=v2::(!v);
                                aux q))
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
and interp_funcall (env : env) (co : coroutine) (fc : functioncall) (k : value -> unit) : unit =
        let (f,arg)=fc in 
        let rec aux v arg k = match arg with 
              | [] -> k []
              |h :: t -> v h (fun x -> aux v t (fun tbl -> k (x :: tbl)))
        in 
        let rec  aux_match (env : env) (co : coroutine) (func : Value.func) (args : value list) (k: value -> unit) : unit =
              match func with
              | Print -> let strings = List.map Value.to_string args in
                         let msg = String.concat "\t" strings in
                          print_endline msg; k Value.Nil
              | Closure (nl, envf, b) -> let ht = create_scope nl args in
                                         let nenv = {envf with locals = ht :: envf.locals} in
                                        interp_block nenv co b k
              | CoroutCreate -> let f = Value.as_function (if args = [] then Value.Nil else List.hd args) in
                                let nco = {Value.stat = Dead} in
                                let retco v = aux_match env nco f [v] (fun ret_val -> match nco.stat with
                                  | Running cont -> nco.stat <- Dead; cont ret_val
                                  | _ -> failwith "") in
                                nco.stat <- Suspended retco;
                                k (Value.Coroutine nco)
              | CoroutResume -> let (coroutine, v) = match args with
                                  | [] -> Value.as_coroutine Value.Nil, Value.Nil
                                  | [a] -> Value.as_coroutine a, Value.Nil
                                  | a::b::_ -> Value.as_coroutine a, b
                      in
                      (match coroutine.stat with
                      | Suspended continuation ->
                          coroutine.stat <- Running k;
                          continuation v;
                      | _ -> failwith "")
             | CoroutYield -> let v = if args = [] then Value.Nil else List.hd args in
                               (match co.stat with
                                | Running cont ->
                                  co.stat <- Suspended k; cont v
                                | _ -> failwith "")
             | CoroutStatus -> let cor = Value.as_coroutine (if args = [] then Value.Nil else List.hd args) in
                      match cor.stat with
                      | Dead -> k (Value.String "dead")
                      | Running _ -> k (Value.String "running")
                      | Suspended _ -> k (Value.String "suspended")
          in interp_exp env co f (fun fv ->
              aux (interp_exp env co) arg (fun l ->
              aux_match env co (Value.as_function fv) l k))

and interp_stat (env : env) (co : coroutine) (stat : stat) (k : unit -> unit) : unit = match stat with
  | Nop -> k ()
  | Seq (s1, s2) -> interp_stat env co s1 (fun () -> interp_stat env co s2 k)
  | Assign (v,e) -> (match v with
                  |Name(n)-> interp_exp env co e (fun x-> Value.set_ident env n x; k())
                  |IndexTable(e1,e2)-> interp_exp env co e1 ( fun v1 ->
                                                              interp_exp env co e2 (fun v2 ->
                                                                interp_exp env co e ( fun ve -> 
                                                                  Hashtbl.add (Value.as_table v1) (Value.as_table_key v2) ve; k())))         )
  | FunctionCall fc -> interp_funcall env co fc (fun _ -> k ())
  | WhileDoEnd (e, s) ->
      let rec while_aux () =
        interp_exp env co e (fun v ->
          if Value.as_bool v then interp_stat env co s while_aux
          else k ())
      in while_aux ()
  | If (e, s1, s2) ->
      interp_exp env co e (fun v ->
        if Value.as_bool v then interp_stat env co s1 k
        else interp_stat env co s2 k)




let run ast =
  let coroutine : (Value.tkey, Value.t) Hashtbl.t = Hashtbl.create 4 in
  Hashtbl.add coroutine (KString "create") (Value.Function CoroutCreate);
  Hashtbl.add coroutine (KString "yield")  (Value.Function CoroutYield);
  Hashtbl.add coroutine (KString "mini_resume") (Value.Function CoroutResume);
  Hashtbl.add coroutine (KString "status") (Value.Function CoroutStatus);
  let globals : (string, Value.t) Hashtbl.t = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Function Print);
  Hashtbl.add globals "coroutine" (Table coroutine);
  let env = Value.{ globals; locals = [] } in
  let coroutine = {Value.stat = Running (fun _ -> ())} in
  interp_block env coroutine ast (fun _ -> ())