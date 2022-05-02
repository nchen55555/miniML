(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string

  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env)

    let lookup (env : env) (varname : varid) : value =
      try !(List.assoc varname env)
      with Not_found -> raise (EvalError "Varid in Env not found")

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      try 
        let _ = lookup env varname in 
        (varname, loc) :: List.remove_assoc varname env
      with EvalError _ -> (varname, loc) :: env

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with 
      | Val exp -> exp_to_concrete_string exp
      | Closure (exp, env) -> exp_to_concrete_string exp ^ (if printenvp then env_to_string env else "")

    and env_to_string (env : env) : string =
      let env_string = "" in 
      let rec helper (env: env) (str: string) : string =  
        match env with 
        | hd :: tl -> let var, v = hd in 
                      (match !v with 
                      | Val exp as all -> var ^ " -> " ^ value_to_string all 
                      | Closure (exp, env) -> var ^ " -> " ^ "[" ^ helper env str ^ 
                                              " ⊢ " ^ exp_to_concrete_string exp ^ "]") ^ 
                                              (if tl = [] then "" else "; " ^ helper tl str) 
        | [] -> ""  
      in helper env env_string 
  end
;;

(* let env1 = extend (empty()) "x" (ref (Val (Num (1))));;
let sub2 = Fun("x", Binop(Plus, Var ("x"), Num (2)));;
let env2 = extend (empty()) "f" (ref (Closure (sub2, env1)));;
let env3 = extend env2 "y" (ref (Val (Num (5))));; *)


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   


(*let rec eval_h (exp: expr) (env: Env.env) (eval_type: int): expr = 
  match exp with 
  | Num _ | Bool _ -> exp
  | Unop (u, e) -> (match eval_h e env eval_type with 
                   | Num num -> Num(~-num)
                   | _ -> raise (EvalError ""))

  | Binop (b, e1, e2) -> (match b, (eval_h e1 env eval_type), (eval_h e2 env eval_type) with  
                         | Equals, Num p, Num q -> Bool (p = q)
                         | Equals, Bool p, Bool q -> Bool (p = q)
                         | LessThan, Num p, Num q -> Bool (p = q)
                         | LessThan, Bool p, Bool q -> Bool (p < q)
                         | Plus, Num p, Num q -> Num (p + q)
                         | Minus, Num p, Num q -> Num (p - q)
                         | Times, Num p, Num q -> Num (p * q)  
                         | _ -> raise (EvalError "Binop"))                          
  | Conditional (e1, e2, e3) -> (match eval_h e1 env eval_type with 
                                | Bool cond -> if cond then eval_h e2 env eval_type else eval_h e3 env eval_type
                                | _ -> raise (EvalError "Conditional"))
  | Raise | Unassigned -> raise (EvalError "Unrecognized expression") (* CAN WE ASSUME A CLOSURE WILL NEVER BE RETURNED BACK *)
  | _ -> if eval_type = 1 then match eval_s exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 2 then match eval_d exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 3 then match eval_l exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else raise (EvalError "Wrong Helper")

and eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_h_s (exp : expr): expr = 
  match exp with 
  | Var v -> raise (EvalError "Unbound Variable")
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> eval_h exp (Env.empty()) 1
  | Fun (v, e) as exp -> exp
  | Let (var, e1, e2) -> eval_h_s (subst var (eval_h_s e1) e2) 
  | Letrec (var, e1, e2) -> let v_d = eval_h_s e1 in 
                            eval_h_s (subst var (eval_h_s (subst var (Letrec (var, v_d, Var var)) v_d)) e2) 
  | App (e1, e2) -> (match (eval_h_s e1), (eval_h_s e2) with
                    | Fun (var, e), expr -> eval_h_s (subst var expr e) 
                    | _ -> raise (EvalError "Function Application"))
  in Env.Val (eval_h_s exp)
  
  and eval_d (exp : expr) (env : Env.env) : Env.value = 
  match exp with 
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 2)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Val exp
  | Let (var, e1, e2)
  | Letrec (var, e1, e2) -> eval_d e2 (Env.extend env var (ref (eval_d e1 env)))
  | App (e1, e2) -> (match (eval_d e1 env), (eval_d e2 env) with
                    | Val(Fun (var, e)), expr -> eval_d e (Env.extend env var (ref expr)) 
                    | _ -> raise (EvalError "Function Application"))
       
and eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 3)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Env.close exp env (* double check - lack of closure *)
  | Let (var, e1, e2) -> eval_l e2 (Env.extend env var (ref (eval_l e1 env))) 
  | Letrec (var, e1, e2) -> let temp = ref (Env.Val(Unassigned)) in 
                            let env_x = Env.extend env var temp
                            in temp := (eval_l e1 env_x); eval_l e2 env_x (* WORKS! *) 
  | App (e1, e2) -> (match (eval_l e1 env), (eval_l e2 env) with
                    | Closure (exp, c_env), value_d -> (match exp with 
                                                       | Fun (v, e) -> eval_l e (Env.extend c_env v (ref value_d))
                                                       | _ -> raise (EvalError "Function Application"))
                    | _ -> raise (EvalError "Function Application"))
  ;; *)

 let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

let rec eval_h (exp: expr) (env: Env.env) (eval_type: int): expr = 
  match exp with 
  | Num _ | Bool _ -> exp
  | Unop (u, e) -> (match u, eval_h e env eval_type with 
                   | Negate, Num num -> Num(~-num)
                   | Deref, Unop(Ref, expr) -> expr
                   | Ref, expr -> Unop(Ref, expr) (* double check *)
                   | _ -> raise (EvalError ""))

  | Binop (b, e1, e2) -> (match b, (eval_h e1 env eval_type), (eval_h e2 env eval_type) with  
                         | Equals, Num p, Num q -> Bool (p = q)
                         | Equals, Bool p, Bool q -> Bool (p = q)
                         | LessThan, Num p, Num q -> Bool (p = q)
                         | LessThan, Bool p, Bool q -> Bool (p < q)
                         | Plus, Num p, Num q -> Num (p + q)
                         | Minus, Num p, Num q -> Num (p - q)
                         | Times, Num p, Num q -> Num (p * q)  
                         | Assign, Unop(Ref, Num p), Num q -> Unit
                         | Assign, Unop(Ref, Bool p), Bool q -> Unit
                         | _ -> raise (EvalError "Binop"))                          
  | Conditional (e1, e2, e3) -> (match eval_h e1 env eval_type with 
                                | Bool cond -> if cond then eval_h e2 env eval_type else eval_h e3 env eval_type
                                | _ -> raise (EvalError "Conditional"))
  | Raise | Unassigned -> raise (EvalError "Unrecognized expression") (* CAN WE ASSUME A CLOSURE WILL NEVER BE RETURNED BACK *)
  | Unit -> Unit
  | _ -> if eval_type = 1 then match eval_s exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 2 then match eval_d exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 3 then match eval_l exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else raise (EvalError "Wrong Helper")

and eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_h_s (exp : expr): expr = 
  match exp with 
  | Var v -> raise (EvalError "Unbound Variable")
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> eval_h exp (Env.empty()) 1
  | Fun (v, e) as exp -> exp
  | Let (var, e1, e2) -> eval_h_s (subst var (eval_h_s e1) e2) 
  | Letrec (var, e1, e2) -> let v_d = eval_h_s e1 in 
                            eval_h_s (subst var (eval_h_s (subst var (Letrec (var, v_d, Var var)) v_d)) e2) 
  | App (e1, e2) -> (match (eval_h_s e1), (eval_h_s e2) with
                    | Fun (var, e), expr -> eval_h_s (subst var expr e) 
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> raise (EvalError "Unit")
  in Env.Val (eval_h_s exp)
  
  and eval_d (exp : expr) (env : Env.env) : Env.value = 
  match exp with 
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 2)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Val exp
  | Let (var, e1, e2)
  | Letrec (var, e1, e2) -> eval_d e2 (Env.extend env var (ref (eval_d e1 env)))
  | App (e1, e2) -> (match (eval_d e1 env), (eval_d e2 env) with
                    | Val(Fun (var, e)), expr -> eval_d e (Env.extend env var (ref expr)) 
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> raise (EvalError "Unit")
       
and eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 3)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Env.close exp env (* double check - lack of closure *)
  | Let (var, e1, e2) -> (match e1 with 
                         | Binop (Assign, p, q) -> let temp = ref (eval_l p env) in 
                                                    (match p with 
                                                    | Var v -> let env_x = Env.extend env v temp 
                                                              in temp := eval_l (Unop (Ref, q)) env_x; 
                                                              eval_l e2 (Env.extend env_x var (ref (Env.Val(Unit)))) 
                                                    | _ -> Val(Unit)) (* double check here *)
                          | _ -> eval_l e2 (Env.extend env var (ref (eval_l e1 env)))) 
  | Letrec (var, e1, e2) -> let temp = ref (Env.Val(Unassigned)) in 
                            let env_x = Env.extend env var temp
                            in temp := (eval_l e1 env_x); eval_l e2 env_x (* WORKS! *) 
  | App (e1, e2) -> (match (eval_l e1 env), (eval_l e2 env) with
                    | Closure (exp, c_env), value_d -> (match exp with 
                                                       | Fun (v, e) -> eval_l e (Env.extend c_env v (ref value_d))
                                                       | _ -> raise (EvalError "Function Application"))
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> Val exp
  ;; 



(*let rec eval_h (exp: expr) (env: Env.env) (eval_type: int): expr = 
  match exp with 
  | Num _ | Bool _ -> exp
  | Unop (u, e) -> (match u, eval_h e env eval_type, eval_type with 
                   | Negate, Num num, _ -> Num(~-num)
                   (*| Deref, Unop(Ref, expr), 3 -> expr
                   | Ref, expr, 3 -> Unop(Ref, expr) (* double check *)*)
                   | _ -> raise (EvalError "Unop")) (* check because I don't think it can evaluate anything unless dereferenced *)
  | Binop (b, e1, e2) -> (match b, (eval_h e1 env eval_type), (eval_h e2 env eval_type) with  
                         | Equals, Num p, Num q -> Bool (p = q)
                         | Equals, Bool p, Bool q -> Bool (p = q)
                         | LessThan, Num p, Num q -> Bool (p = q)
                         | LessThan, Bool p, Bool q -> Bool (p < q)
                         | Plus, Num p, Num q -> Num (p + q)
                         | Minus, Num p, Num q -> Num (p - q)
                         | Times, Num p, Num q -> Num (p * q)  
                         (*| Assign, Unop(Ref, Num p), Num q -> Unit
                         | Assign, Unop(Ref, Bool p), Bool q -> Unit*)
                         | _ -> raise (EvalError "Binop"))   (* IF ANY ERRORS - PROBABLY HERE *)                 
  | Conditional (e1, e2, e3) -> (match eval_h e1 env eval_type with 
                                | Bool cond -> if cond then eval_h e2 env eval_type else eval_h e3 env eval_type
                                | _ -> raise (EvalError "Conditional"))
  | Raise | Unassigned -> raise (EvalError "Unrecognized expression") (* CAN WE ASSUME A CLOSURE WILL NEVER BE RETURNED BACK *)
  | _ -> if eval_type = 1 then match eval_s exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 2 then match eval_d exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else if eval_type = 3 then match eval_l exp env with | Val v -> v | _ -> raise (EvalError "Non-closure")
         else raise (EvalError "Wrong Helper")

and eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_h_s (exp : expr): expr = 
  match exp with 
  | Var v -> raise (EvalError "Unbound Variable")
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> eval_h exp (Env.empty()) 1
  | Fun (v, e) as exp -> exp
  | Let (var, e1, e2) -> eval_h_s (subst var (eval_h_s e1) e2) 
  | Letrec (var, e1, e2) -> let v_d = eval_h_s e1 in 
                            eval_h_s (subst var (eval_h_s (subst var (Letrec (var, v_d, Var var)) v_d)) e2) 
  | App (e1, e2) -> (match (eval_h_s e1), (eval_h_s e2) with
                    | Fun (var, e), expr -> eval_h_s (subst var expr e) 
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> raise (EvalError "Unit")
  in Env.Val (eval_h_s exp)
  
  and eval_d (exp : expr) (env : Env.env) : Env.value = 
  match exp with 
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 2)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Val exp
  | Let (var, e1, e2)
  | Letrec (var, e1, e2) -> eval_d e2 (Env.extend env var (ref (eval_d e1 env)))
  | App (e1, e2) -> (match (eval_d e1 env), (eval_d e2 env) with
                    | Val(Fun (var, e)), expr -> eval_d e (Env.extend env var (ref expr)) 
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> raise (EvalError "Unit")
       
and eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with                          
  | Num _ | Bool _ | Unop _ | Binop _ | Conditional _ | Raise | Unassigned -> Val (eval_h exp env 3)
  | Var v -> Env.lookup env v
  | Fun (v, e) as exp -> Env.close exp env (* double check - lack of closure *)
  | Let (var, e1, e2) -> (match e1 with 
                         | Binop (Assign, p, q) -> let temp = ref (eval_l p env) in 
                                                    (match p with 
                                                    | Var v -> let env_x = Env.extend env v temp 
                                                              in temp := eval_l (Unop (Ref, q)) env_x; 
                                                              eval_l e2 (Env.extend env_x var (ref (Env.Val(Unit)))) 
                                                    | _ -> Val(Unit)) (* double check here *)
                         | _ -> let env_x = (Env.extend env var (ref (eval_l e1 env))) in eval_l e2 env_x)
  | Letrec (var, e1, e2) -> let temp = ref (Env.Val(Unassigned)) in 
                            let env_x = Env.extend env var temp
                            in temp := (eval_l e1 env_x); eval_l e2 env_x (* WORKS! *) 
  | App (e1, e2) -> (match (eval_l e1 env), (eval_l e2 env) with
                    | Closure (exp, c_env), value_d -> (match exp with 
                                                       | Fun (v, e) -> eval_l e (Env.extend c_env v (ref value_d))
                                                       | _ -> raise (EvalError "Function Application"))
                    | _ -> raise (EvalError "Function Application"))
  | Unit -> Val exp
  ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;*)
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_l ;;