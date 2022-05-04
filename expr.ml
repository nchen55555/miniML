(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Deref
  | Ref
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | Assign
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | Unit                                 (* nothing is returned *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Var v -> SS.singleton v 
  | Num _ | Bool _| Raise | Unassigned | Unit -> SS.empty 
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2)-> SS.union (free_vars e1) (free_vars e2) 
  | Conditional (e1, e2, e3) -> SS.union (free_vars e1) (SS.union (free_vars e2) (free_vars e3))
  | Fun (var, e) -> SS.remove var (free_vars e) 
  | Let (var, e1, e2) -> SS.union (free_vars e1) (SS.remove var (free_vars e2))
  | Letrec (var, e1, e2) -> SS.remove var (SS.union (free_vars e1) (free_vars e2)) ;;

let test_free_vars () = 
  let exp = Num(3) in 
  let set = SS.empty in 
  assert (free_vars exp = set); 
  let exp = Var("x") in 
  assert (free_vars exp = SS.add "x" set); 
  let exp = Let("x", Num(3), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) in 
  assert (free_vars exp = SS.add "f" SS.empty); 
  let exp = Let("x", Var("x"), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) in 
  assert (free_vars exp = SS.add "f" (SS.add "x" SS.empty)); 
  let exp = Let("x", Var("y"), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) in 
  assert (free_vars exp = SS.add "f" (SS.add "y" SS.empty)); 
  let exp = Let("x", Fun("y", Var("x")), Var("x")) in 
  assert (free_vars exp = SS.add "x" SS.empty); 
  let exp = Let("f", Fun("x", Binop(Plus, Var("x"), Var("y"))), App(Var("f"), Var("x"))) in 
  assert (free_vars exp = SS.add "x" (SS.add "y" SS.empty));
  let exp = App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(3)) in 
  assert (free_vars exp = SS.empty); 
  let exp = Let("f", Fun("x", Fun("y", Conditional(Binop(LessThan, Var("x"), Var("y")), Var("y"), App(App(Var("f"), Var("y")), Var("x"))))), App(App(Var("f"), Num(0)), Num(1))) in 
  assert (free_vars exp = SS.add "f" SS.empty); 
  let exp = App(Fun("x", Var("x")), Fun("y", Var("x"))) in 
  assert (free_vars exp = SS.add "x" SS.empty); 
  let exp = Fun("x", App(Fun("y", Var("x")), Var("x"))) in 
  assert (free_vars exp = SS.empty)
   ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)

let new_varname : unit -> string =
  let counter = ref ~-1 in 
  fun () -> 
  incr counter;
  "var" ^ string_of_int !counter

let test_new_varname () = 
  assert (new_varname () = "var0"); 
  assert (new_varname () = "var1"); 
  assert (new_varname () = "var2")


(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)

let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr = (* currying?? with helper *)
  match exp with 
  | Var v -> if v = var_name then repl else exp 
  | Num _ | Bool _ -> exp 
  | Unop (u, e) -> Unop (u, subst var_name repl e) (* make sure substitution sematnics aren't involved? *)
  | Binop (b, e1, e2) -> Binop (b, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) -> Conditional ((subst var_name repl e1), (subst var_name repl e2), (subst var_name repl e3)) (* CHECK *)
  | Fun (var, e) -> if var = var_name then exp
                    else if (SS.mem var (free_vars repl)) 
                    then let var_new = new_varname () in 
                    Fun (var_new, subst var (Var var_new) (subst var_new repl e)) (* fun z -> P [y ↦ z][x ↦ Q] *)
                    else Fun (var, subst var_name repl e) 
  | Let (var, e1, e2) -> if var = var_name then Let(var, subst var_name repl e1, e2) (* return the expression??? *)
                        else if (SS.mem var (free_vars repl)) 
                        then let var_new = new_varname () in 
                        Let (var_new, subst var_name repl e1, subst var (Var var_new) (subst var_name repl e2)) (*  (let y = D in B) [x ↦ Q] = (let z = D [x ↦ Q] in B [y ↦ z] [x ↦ Q]) (if y is a FV in Q) *)
                        else Let (var, subst var_name repl e1, subst var_name repl e2) (* (let y = D in B) [x ↦ Q] = (let y = D [x ↦ Q] in B [x ↦ Q]) (if y is not a FV in Q) *)
  | Letrec (var, e1, e2) -> if var = var_name then Letrec(var, subst var_name repl e1, e2) (* return the expression??? *)
                        else if (SS.mem var (free_vars repl)) 
                        then let var_new = new_varname () in 
                        Letrec (var_new, subst var_name repl e1, subst var (Var var_new) (subst var_new repl e2)) (*  (let y = D in B) [x ↦ Q] = (let z = D [x ↦ Q] in B [y ↦ z] [x ↦ Q]) (if y is a FV in Q) *)
                        else Letrec (var, subst var_name repl e1, subst var_name repl e2) (* (let y = D in B) [x ↦ Q] = (let y = D [x ↦ Q] in B [x ↦ Q]) (if y is not a FV in Q) *)
  | Raise | Unassigned | Unit -> exp
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2) (*(Q R)[x ↦ P] = Q[x ↦ P] R[x ↦ P] *)


let test_subst () = 
  let exp = Fun("y", Var("x")) in 
  assert (subst "x" (Num(42)) exp = Fun("y", Num(42)));
  let exp = Binop(Plus, Var("x"), Num(1)) in 
  assert (subst "x" (Num(50)) exp = Binop(Plus, Num(50), Num(1)));
  let exp = Let("x", Binop(Times, Var("y"), Var("y")), Binop(Plus, Var("x"), Var("x"))) in 
  assert (subst "x" (Num(3)) exp = Let("x", Binop(Times, Var("y"), Var("y")), Binop(Plus, Var("x"), Var("x"))));
  let exp = Let("x", Binop(Times, Var("y"), Var("y")), Binop(Plus, Var("x"), Var("x"))) in 
  assert (subst "y" (Num (3)) exp = Let("x", Binop(Times, Num(3), Num(3)), Binop(Plus, Var("x"), Var("x"))));
  let exp = App(Fun("y", Var("x")), Num(21)) in 
  assert (subst "x" (Num(42)) exp = App(Fun("y", Num(42)), Num(21)));
  let exp = Let("x", Binop(Plus, Var("x"), Num(1)), Binop(Plus, Var("x"), Num(2))) in 
  assert (subst "x" (Num(42)) exp = Let("x", Binop(Plus, Num(42), Num(1)), Binop(Plus, Var("x"), Num(2))));
  let exp = Let("x", Num(5), App(Var("f"), Var("y"))) in 
  assert (subst "y" (Binop(Plus, Var("x"), Num(1))) exp =  Let("var3", Num(5), App(Var("f"), Binop(Plus, Var("var3"), Num(1)))))
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)


let binop_to_string_abs (b: binop) = 
  match b with 
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan"
  | Assign -> "Assign"

let unop_to_string_abs (u: unop) = 
  match u with 
  | Negate -> "Negate"
  | Deref -> "Deref"
  | Ref -> "Ref"
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)

let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var(" ^ v ^ ")"                      
  | Num num -> "Num(" ^ string_of_int num ^")"                         
  | Bool b -> Bool.to_string b                       
  | Unop (u, e) -> "Unop(" ^ unop_to_string_abs u ^ ", " ^ 
                    exp_to_abstract_string e ^ ")"                 
  | Binop (b, e1, e2) -> "Binop(" ^ (binop_to_string_abs b) ^ ", " ^ 
                          (exp_to_abstract_string e1) ^ ", " ^
                          (exp_to_abstract_string e2) ^ ")"     
  | Conditional (e1, e2, e3) -> "Conditional(" ^ (exp_to_abstract_string e1) ^ 
                                ", " ^ (exp_to_abstract_string e2) ^ 
                                ", " ^ (exp_to_abstract_string e3) ^ ")"
  | Fun (var, e) -> "Fun(" ^ var ^ ", " ^ exp_to_abstract_string e ^ ")"                 
  | Let (var, e1, e2) -> "Let(" ^ var ^ ", " ^ 
                        (exp_to_abstract_string e1) ^ 
                         ", " ^ (exp_to_abstract_string e2) ^ ")"
  | Letrec (var, e1, e2) -> "Letrec(" ^ var ^ ", " ^ 
                        (exp_to_abstract_string e1) ^ 
                         ", " ^ (exp_to_abstract_string e2) ^ ")"
  | Raise -> "Raise" 
  | Unassigned -> "Unassigned"                          
  | App (e1, e2) -> "App(" ^ (exp_to_abstract_string e1) ^ 
                    ", " ^ (exp_to_abstract_string e2) ^ ")"
  | Unit -> "Unit" ;;


let binop_to_string_con (b: binop) = 
  match b with 
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equals -> " = "
  | LessThan -> " < "
  | Assign -> " := "

let unop_to_string_con (u: unop) = 
  match u with 
  | Negate -> "-"
  | Deref -> "!"
  | Ref -> "ref "

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> v                      
  | Num num -> string_of_int num                         
  | Bool b -> Bool.to_string b                       
  | Unop (u, e) -> unop_to_string_con u ^ exp_to_concrete_string e   
  | Binop (b, e1, e2) -> exp_to_concrete_string e1 ^ binop_to_string_con b ^
                         exp_to_concrete_string e2    
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string e1 ^ "\n"
                                ^ "then " ^ exp_to_concrete_string e2 ^ "\n"
                                ^ "else " ^ exp_to_concrete_string e3
  | Fun (var, e) -> "fun " ^ var ^ " -> " ^ exp_to_concrete_string e                 
  | Let (var, e1, e2) -> "let " ^ var ^ " = " ^ 
                        (exp_to_concrete_string e1) ^ 
                         " in " ^ (exp_to_concrete_string e2)
  | Letrec (var, e1, e2) -> "let rec " ^ var ^ " = " ^ 
                        (exp_to_concrete_string e1) ^ 
                         " in " ^ (exp_to_concrete_string e2)
  | Raise -> "parse error" 
  | Unassigned -> "Unassigned"                          
  | App (e1, e2) -> (exp_to_concrete_string e1) ^ 
                    "(" ^ (exp_to_concrete_string e2) ^ ")" 
  | Unit -> "()" ;;

let run_tests () = 
  test_free_vars ();
  test_new_varname ();
  test_subst ()
