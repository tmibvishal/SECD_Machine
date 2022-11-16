exception SECDSTACKMACHINEEXCEPRION;;
exception COMPILATIONEXCEPTION;;
exception CANTFINDINENVIRONMENT;;

type exp =
        Var of string						  (* variable definition *)
			| Lambda of string*exp			(* abstraction definition *)
			| Call of exp*exp 					(* function call definition *)
      | Integer of int
      | Add of exp*exp
      | Subt of exp*exp
      | Mult of exp*exp			
      | Boolean of bool	          (* binary operations *)				
      | And of exp*exp
      | Or of exp*exp
      | Equals of exp*exp         (* comparisons *)
      | Gtz of exp*exp;;				
      
type opcode = 
        VAR of string
      | INTEGER of int
      | BOOLEAN of bool
      | LAMBDA of string * (opcode list)
      | ARG of opcode list
      | APPLY | ADD | MULT | SUBT | AND | OR | EQUALS | GTZ;;

type answer = B of bool | I of int | S of string | Vcl of environment*string*(opcode list)
and environment = (string * answer) list;;

type dumptype = ((answer list) * environment * (opcode list)) list;;

let rec compile e = match e with
				| Integer (i) -> [INTEGER (i)]
        | Boolean (b) -> [BOOLEAN b]
        | Var(x) -> [VAR x]
        | Add(e1,e2) -> (compile e1)@(compile e2)@[ADD]
        | Subt(e1,e2) -> (compile e1)@(compile e2)@[SUBT]
				| Mult(e1,e2) -> (compile e1)@(compile e2)@[MULT]
        | And(e1,e2) -> (compile e1)@(compile e2)@[AND]
        | Or(e1,e2) -> (compile e1)@(compile e2)@[OR]
				| Equals(e1,e2) -> (compile e1)@(compile e2)@[EQUALS]
				| Gtz(e1,e2) -> (compile e1)@(compile e2)@[GTZ]
				| Lambda(x,e1) -> [LAMBDA(x,(compile e1))]
				| Call(e1,e2) -> (compile e1)@[ARG(compile e2)]@[APPLY]
        | _ -> raise COMPILATIONEXCEPTION;;
        
let rec findInEnv (env: environment) (x: string) : answer = 
        match env with
          [] -> raise CANTFINDINENVIRONMENT
        | (x',a)::env' -> if (x' = x) then a else findInEnv env' x;;

let rec secdExecute (stk: answer list) (env: environment) (code: opcode list) (dump: dumptype) =
  match (stk, env, code, dump) with
  [a], _, [], [] -> a (* oplist empty so just return last ans in stack *)
  | (s, e, [], (s',e',c')::ds) -> secdExecute ( s@s') e' c' ds
  | (s, e, VAR(x)::c, d) -> secdExecute ((findInEnv e x)::s) e c d
  | (s, e, LAMBDA(x, cl)::c, d) -> secdExecute (Vcl(e, x, cl)::s) e c d
  | (a::Vcl(e', x, cl)::s, e, APPLY::c, d) -> secdExecute [] ((x,a)::e') cl ((s,e,c)::d)
  | (s, e, ARG(l)::c, d) -> secdExecute s e (l@c) d		
  | (s, e, INTEGER(i)::c, d) -> secdExecute (I(i)::s) e c d
  | (s, e, BOOLEAN(b)::c, d) -> secdExecute (B(b)::s) e c d
  | (I(a2)::I(a1)::s, e, ADD::c, d) -> secdExecute ((I(a1+a2))::s) e c d
  | (I(a2)::I(a1)::s, e, SUBT::c, d) -> secdExecute ((I(a1-a2))::s) e c d
  | (I(a2)::I(a1)::s, e, MULT::c, d) -> secdExecute ((I(a1*a2))::s) e c d
  | (B(b2)::B(b1)::s, e, AND::c, d) -> secdExecute ((B(b1 && b2))::s) e c d
  | (B(b2)::B(b1)::s, e, OR::c, d) -> secdExecute ((B(b1 || b2))::s) e c d
  | (a2::a1::s, e, EQUALS::c, d) -> if (a1=a2) then secdExecute ((B(true))::s) e c d else secdExecute ((B(false))::s) e c d
  | ((I(i))::s, e, GTZ::c, d) -> if (i>0) then secdExecute ((B(true))::s) e c d else secdExecute ((B(false))::s) e c d	
  |_ -> raise SECDSTACKMACHINEEXCEPRION;;

let secdRunInitial env e = secdExecute [] env (compile e) [];;

(* Testing *)
let env1 = ["x", I(20)];;
let env2 = [("x", I(1)); ("y", I(-1)); ("b", B(true))];;
