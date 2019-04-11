(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns resulting configuration
    *)                                                       
    let to_func op =
        let bti   = function true -> 1 | _ -> 0 in
        let itb b = b <> 0 in
        let (|>) f g   = fun x y -> f (g x y) in
        match op with
        | "+"  -> (+)
        | "-"  -> (-)
        | "*"  -> ( * )
        | "/"  -> (/)
        | "%"  -> (mod)
        | "<"  -> bti |> (< )
        | "<=" -> bti |> (<=)
        | ">"  -> bti |> (> )
        | ">=" -> bti |> (>=)
        | "==" -> bti |> (= )
        | "!=" -> bti |> (<>)
        | "&&" -> fun x y -> bti (itb x && itb y)
        | "!!" -> fun x y -> bti (itb x || itb y)
        | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
  
      let rec eval env ((st, i, o, r) as conf) expr =      
        match expr with
        | Const c -> (st, i, o, Some c)
        | Var v -> (st, i, o, Some (State.eval st v))
        | Binop (op, x, y) ->
          let (st, i, o, Some r1) = eval env conf x in
          let (st, i, o, Some r2) = eval env (st, i, o, None) y in
          (st, i, o, Some to_func op r1 r2)
        | Call (fun_name, fun_args) ->
           let eval_args (conf, acc) arg =
             let ((_, _, _, Some v) as new_conf) = eval env conf arg in
             (new_conf, v::acc) in
           let new_conf, arg_vals = List.fold_left eval_args (conf, []) fun_args in
           env#definition env fun_name (List.rev arg_vals) new_conf
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
    ostap (                                      
      parse: expr;
        expr:
          !(Ostap.Util.expr
              (fun x -> x)
              (Array.map (fun (a, s) -> a, 
                             List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                          ) 
              [|
                `Lefta, ["!!"];
                `Lefta, ["&&"];
                `Nona , [">="; ">"; "<="; "<"; "!="; "=="];
                `Lefta, ["+"; "-"];
                `Lefta, ["*"; "/"; "%"];
              |]
              )
              primary
            );
        primary: n:DECIMAL { Const n } 
        | fun_name:IDENT -"(" fun_args:!(Util.list0)[parse] -")" { Call (fun_name, fun_args) } 
        | x:IDENT { Var x } | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
    let rec eval env ((state, input, output, r) as conf) k statement =
        let meta k s =
          match k with
          | Skip -> s
          | _ -> Seq (s, k)
        in
        match statement with
          | Read x -> 
          begin
          (match input with
                        | head::tail -> eval env ((State.update x head state), tail, output, r) Skip k
                        | _ -> failwith "cannot perform")
          end
          | Write expr -> 
            let (new_state, new_input, new_output, Some n) = Expr.eval env conf expr in
            eval env (new_state, new_input, new_output @ [n], r) Skip k
          | Assign (x, expr) -> 
            let (new_state, new_input, new_output, Some n) = Expr.eval env conf expr in
            eval env (State.update x n new_state, new_input, new_output, r) Skip k
          | Seq (st1, st2) -> 
            eval env conf (meta k st2) st1
          | If (e, thS, eS) -> 
            let (new_state, new_input, new_output, Some n) = Expr.eval env conf e in
            eval env (new_state, new_input, new_output, r) k (if n != 0 then thS else eS)
          |  While (e, s) ->
            let (new_state, new_input, new_output, Some n) = Expr.eval env conf e in
            let new_conf = (new_state, new_input, new_output, r) in
            if n != 0 then eval env new_conf (meta k statement) s else eval env new_conf Skip k
          | Repeat (rS, e) -> 
            eval env conf (meta k (While (Expr.Binop ("==", rS, Expr.Const 0), e))) e
          | Skip -> 
            begin
              match k with
              | Skip -> conf
              | _ -> eval env conf Skip k
            end
          | Call (name, args) ->
            eval env (Expr.eval env conf (Expr.Call (name, args))) Skip k
          | Return opt ->
            begin
              match opt with
              | Some res -> Expr.eval env conf res
              | _ -> (state, input, output, None)
            end
         
    (* Statement parser *)
    ostap (
      parse    : seq | stmt;
        stmt     : read | write | assign | skip | if' | while' | for' | repeat | fun_call | return;
        read     : %"read" -"(" x:IDENT -")" { Read x };
        write    : %"write" -"(" e:!(Expr.parse) -")" { Write e };
        assign   : x:IDENT -":=" e:!(Expr.parse) { Assign (x, e) };
        seq      : s1:stmt -";" s2:parse { Seq(s1, s2) };
        skip     : %"skip" { Skip };
        if'      : %"if" cond:!(Expr.parse)
                   %"then" then_body:parse
                    elif_bodies :(%"elif" !(Expr.parse) %"then" parse)*
                    else_body :(%"else" parse)? %"fi"
                    {
                      let else_body' = match else_body with
                      | Some t -> t
                      | None -> Skip
                      in
                      let elif_else_bodies = List.fold_right (fun (e', t') t -> If (e', t', t)) elif_bodies else_body' in
                      If (cond, then_body, elif_else_bodies)
                    };
        while'   : %"while" cond:!(Expr.parse) %"do" while_body:parse %"od"
                    { While (cond, while_body) };
        for'     : %"for" for_asgn:parse "," cond:!(Expr.parse) "," for_upd:parse %"do" for_body:parse %"od"
                    { Seq (for_asgn, While (cond, Seq (for_body, for_upd))) };
        repeat   : %"repeat" repeat_body:parse %"until" cond:!(Expr.parse)
                    { Repeat (cond, repeat_body) };
        fun_call : fun_name:IDENT -"(" fun_args:!(Expr.parse)* -")" { Call(fun_name, fun_args) };
        return   : %"return" res:!(Expr.parse)? { Return res }
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (     
      arg  : IDENT;
        parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
           locs:(%"local" !(Util.list arg))?
          "{" body:!(Stmt.parse) "}" {
          (name, (args, (match locs with None -> [] | Some l -> l), body))
        }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =                                                                      
           let xs, locs, s      =  snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
