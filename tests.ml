(* tests for expr file *)
let _ = Expr.run_tests () ;;
(* tests for env module *)
let _ = Evaluation.Env.env_lookup_tester ()
let _ = Evaluation.Env.env_extend_tester ()
(* tests for evaluation *)
let _ = Evaluation.run_tests ()