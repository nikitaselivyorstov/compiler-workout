open GT       
       
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
type config = int list * Syntax.Stmt.config

let evalInstruction configuration instructions = 
		let (stack, conf) = configuration in
		let (state, input, output) = conf in
		
		match instructions with
		| BINOP operator -> (match stack with
			| y::x::left -> ([(Syntax.Expr.calculate operation) x y] @ left, conf))
	    | CONST value -> ([value] @ stack, conf)                 
		| READ -> (match input with
			| x::left -> ([x] @ stack, (state, left, output)))
		| WRITE -> (match stack with
			| x::left -> (left, (state, input, output @ [x])))
		| LD  variable -> ([state variable] @ stack, conf)
		| ST  variable -> (match stack with
			| x::left -> (left, (Syntax.Expr.update variable x state, input, output)))


(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
(*let eval _ = failwith "Not yet implemented"*)
let eval conf programm = List.fold_left evalInstruction conf programm

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

(*let compile _ = failwith "Not yet implemented"*)

let rec compileExp expression = match expression with
	  | Syntax.Expr.Const constant -> [CONST constant]
	  | Syntax.Expr.Var variable -> [LD variable]
	  | Syntax.Expr.Binop (operator, left, right) -> (compileExp left) @ (compileExp right) @ [BINOP operator];;
	
	let rec compile statement = match statement with
	  | Syntax.Stmt.Read variable -> [READ; ST variable]
	  | Syntax.Stmt.Write expression -> (compileExp expression) @ [WRITE]
	  | Syntax.Stmt.Assign (variable, expression) -> (compileExp expression) @ [ST variable]
	  | Syntax.Stmt.Seq (current, following) -> (compile current) @ (compile following);;
