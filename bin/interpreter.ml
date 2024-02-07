;;open Ast

(* Interpreter -------------------------------------------------------------- *)

(* We can "interpret" a SIMPLE program by giving it a meaning in terms of 
 * OCaml operations.  One key question is how to represent the _state_ of
 * the SIMPLE program.  Out intuition tells us that it should be a map
 * that sends variables to their (current) values.  There are many ways that
 * we could represent such a state.  Here, we use OCaml's functions.
*)
type state = var -> int64

(* The initial state maps every variable to 0 *)
let init_state : state =
  fun _ -> 0L

(* We can update an old state [s] to one that maps `x` to `v` but is otherwise
 * unchanged by building a new function like this: *)
let update (s:state) (x:var) (v:int64) =
  fun y -> if x = y then v else s y

(* Looking up the value of a variable in a state is easy: *)
let lookup (s:state) (x:var) : int64 = s x

(* To interpret an expression in a given state, we recursively compute the
 * values of subexpressions and then combine them according to the operation.
 *)
let rec interpret_exp (s:state) (e:exp) : int64 =
  match e with
  | Var x -> s x
  | Const n -> n
  | Add (e1, e2) -> Int64.add (interpret_exp s e1) (interpret_exp s e2)
  | Mul (e1, e2) -> Int64.mul (interpret_exp s e1) (interpret_exp s e2)
  | Neg e1 -> Int64.neg (interpret_exp s e1)

exception Halt of int64

(* To interpret a command, we write an OCaml program that manipulates that 
 * state as appropriate.  The result of running a command is a final state
 * or a "Halt" exception, indicating that the program has returned a value.
 * 
 * Note that `WhileLZ` "unfolds" the loop into a conditional that either 
 * runs the loop body once and then continues as another `WhileLZ`, or just 
 * Skip.
 *
 * Note that the SIMPLE sequence of two commands is interpreted by the
 * sequencing of OCaml's `let` binding construct.
 *)
let rec interpret_cmd (s:state) (c:cmd) : state =
  match c with
  | Skip -> s
  | Assn (x, e) ->
     let v = interpret_exp s e in
     update s x v
  | IfLZ (e1, c1, c2) ->
     if Int64.compare (interpret_exp s e1) Int64.zero < 0
     then interpret_cmd s c1
     else interpret_cmd s c2
  | WhileLZ (e, c) ->
     if Int64.compare (interpret_exp s e) Int64.zero < 0 then
       interpret_cmd s (Seq(c, WhileLZ (e, c)))
     else
       s
  | Seq (c1, c2) ->
     let s1 = interpret_cmd s c1 in
     interpret_cmd s1 c2
  | Return e ->
     raise (Halt (interpret_exp s e))

let fact x =
  let n = "n" in
  let ans = "ans" in
  Seq (Seq (Assn (n, Const x),
            Seq (Assn (ans, Const 1L),
                 WhileLZ(Neg (Var n),
                         Seq (Assn(ans, Mul(Var ans, Var n)),
                              Assn(n, Add(Var n, Const (-1L))))))),
       Return (Var ans))

let answer program =
  try
    ignore (interpret_cmd init_state program "ans");
    0L;
  with Halt x -> x 
