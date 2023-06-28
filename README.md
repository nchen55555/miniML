# Introduction 

miniML is a meta-circular interpreter in OCaml for a Turing-complete, ML-based language supporting atomic data types, lazy expressions, and lexically scoped environment semantics.

On top of that, I implemented a couple of extensions. First, I added the lexical semantics environment to evaluate expressions. Second, I augmented the range of acceptable inputs to the parser and evaluator such that mutable state and imperative programming were now represented. Lastly, I included floats in the acceptable inputs to the parser as well as division.

## Lexical Environments 

To begin, I modified the scope of environmental semantics to include not just
dynamical but also lexical. This is showcased in the evaluation.ml file in the
the eval l code. In the written code, I specifically used and statements to try
to abstract the code as eval s, eval d and eval l have similar implementa-
tions for Num, Bool, Unop, Binop, Conditional, Unit, Raise, and Unassigned.
However, where eval l mostly diverges is in function and function applica-
tion. Rather than returning the function, in a lexical environment, evaluation
should return a closure wrapping up the function in the current environment.
This way, during function application, the right environment will be applied
to the function. We see this in the match statement for functions, whereby
rather than repeating code from the dynamic environment (Val exp), the lex-
ical environment returns a closure from the Env module such that the match
statement returns Env.close exp env. Of course, during function application
then, rather than matching the evaluated function as a value (as the dynamic
environment does), we match it for a closure and pattern match/evaluate within
the closure’s environment to get the correct expression. <br />
Lastly, unlike dynamic environments which have the same implementa-
tion for let and letrec, the lexical environment must utilize the Unassigned
expression to evaluate letrec statements. 

## Mutable State 

The next extension that I implemented involved references. Specifically, I imple-
mented evaluation rules for the assignment operator :=, the reference operator
ref, and the dereference operator !. Note: I did not implement code for the
sequence operator ; as it was not specified in the suggested extensions. To
develop these extensions, I first incorporated these symbols into the lexical an-
alyzer, adding to the keyword and symbol table in the miniml lex.mll file code. 

("ref", REFERENCE) <br />
(":=", ASSIGN); <br />
("!", DEREFERENCE) <br />

This code was added so that the lexical analyzer would recognize these
different operators when typed into the repl. This was accompanied by ad-
ditions to the parser. Ultimately, I implemented the dereference and reference
operators as instances of unop and the assignment operator as an instance of
binop for assignment priority reasons as well as to simplify the code. <br />

Because mutable states can only be represented in the lexical environ-
ment, any attempt to utilize references in the dynamic environment semantics
or substitution semantics results in an evaluation error. <br />

## Floats and Division 

For my last extension, I implemented Floats and Divisions. Similar to how we
implemented integers within the lexical analyzer and parser, I added floats to the
lexical analyzer through utilizing ocaml’s float of string function as shown
below and adding the respective token and precedence to the parser whereby
TIMES and DIVIDE had the same precedence. <br />


