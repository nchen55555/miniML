(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
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
  | Num _  
  | Bool _ -> SS.empty 
  | Unop (_, e) -> free_vars (e) 
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2) 
  | Conditional (e1, e2, e3) -> SS.union (free_vars e1) (SS.union (free_vars e2) (free_vars e3))
  | Fun (var, e) -> SS.remove var (free_vars e) 
  | Let (var, e1, e2) 
  | Letrec (var, e1, e2) -> SS.union (free_vars e1) (SS.remove var (free_vars e2))
  | Raise 
  | Unassigned -> SS.empty 
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2);;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let rec exp_to_concrete_string (exp : expr) : string =
  failwith "exp_to_concrete_string not implemented"


let binop_to_string (b: binop) = 
  match b with 
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "Less Than"

let unop_to_string (u: unop) = 
  match u with 
  | Negate -> "Negate"
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
    match exp with 
  | Var v -> "Var(" ^ v ^ ")"                      
  | Num num -> "Num(" ^ string_of_int num ^")"                         
  | Bool b -> Bool.to_string b                       
  | Unop (u, e) -> "Unop(" ^ unop_to_string u ^ ", " ^ 
                    exp_to_abstract_string e ^ ")"                 
  | Binop (b, e1, e2) -> "Binop(" ^ (binop_to_string b) ^ ", " ^ 
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
  | Raise -> "parse error" 
  | Unassigned -> "Unassigned"                          
  | App (e1, e2) -> "App(" ^ (exp_to_abstract_string e1) ^ 
                    ", " ^ (exp_to_abstract_string e2) ^ ")" ;;
