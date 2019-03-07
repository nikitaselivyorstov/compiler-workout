open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let evalInstriction configuration instructions = 
		let (stack, config) = configuration in
		let (state, input, output) = config in
	
		match instructions with
		| BINOP operation -> (match stack with
			| y::x::left -> [Syntax.Expr.calculate operation x y] @ left, config)
	    | CONST x -> [x] @ stack, config
		| READ -> (match input with
			| x::left -> [x] @ stack, (state, left, output))
		| WRITE -> (match stack with
			| x::left -> left, (state, input, output @ [x]))
		| LD variable -> [state variable] @ stack, config	
		| ST variable -> (match stack with
			| x::left -> left, (Syntax.Expr.update variable x state, input, output)
	                )

		let eval configuration prog = List.fold_left evalInstriction configuration prog

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compileExp expression = match expression with
	  | Syntax.Expr.Const constant -> [CONST constant]
	  | Syntax.Expr.Var variable -> [LD variable]
	  | Syntax.Expr.Binop (operation, left, right) -> (compileExp left) @ (compileExp right) @ [BINOP operation];;
	
	let rec compile statement = match statement with
	  | Syntax.Stmt.Read variable -> [READ; ST variable]
	  | Syntax.Stmt.Write expression -> (compileExp expression) @ [WRITE]
	  | Syntax.Stmt.Assign (variable, expression) -> (compileExp expression) @ [ST variable]
	  | Syntax.Stmt.Seq (current, following) -> (compile current) @ (compile following);;
