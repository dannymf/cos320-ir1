(* Grammar ------------------------------------------------------------------ *)
(*
  Grammar for a SIMPLE language:

  <exp> ::= 
         |  <var>
         |  <integer contant>
         |  <exp> + <exp>
         |  <exp> * <exp>
         |  -<exp>
         |  (<exp>)

  <cmd> ::= 
         |  skip   (do nothing)
         |  <var> = <exp>
         |  ifLZ <exp> { <cmd> } else { <cmd> }
         |  whileLZ <exp> { <cmd> }
         |  <cmd>; <cmd>
         |  return <exp>
*)

(* Abstract syntax tree (AST) ---------------------------------------------- *)

(* 
 *  OCaml datatypes that we use to represent SIMPLE abstract syntax.
 *  
 *  This is called _abstract syntax_ because it uses the labeled 
 *  tree structure rather than concrete keywords, punctuation marks, etc., 
 *  to represent the program.
 *
 *  For example, the concrete syntax for the following program:
 *                  (3 + x1) * 2
 *  is the tree:
 *      Mul(Add(Const 3L, Var "x1"), Const 2L)
 *)

type var = string

type exp =
  | Var of var                    (* <var> *)
  | Const of int64                (* <integer constant> *)
  | Add of (exp * exp)            (* <exp> + <exp> *)
  | Mul of (exp * exp)            (* <exp> * <exp> *)
  | Neg of exp                    (* -<exp> *)

type cmd =
  | Skip                          (* skip *)          
  | Assn    of var * exp          (* <var> = <exp> *)
  | IfLZ    of exp * cmd * cmd    (* ifLZ <exp> { <cmd> } else { <cmd> } *)
  | WhileLZ of exp * cmd          (* whileLZ <exp> { <cmd> } *)
  | Seq     of cmd * cmd          (* <cmd>; <cmd> *)
  | Return  of exp                (* return <exp> *)

(* Sample expressions -------------------------------------------------------- *)

(* (3 + x1) * 2 *)
let e = Mul(Add(Const 3L, Var "x1"), Const 2L)

(* x1 + (x2 + (x3 + (x4  (x5 + (x6 + (x6 + (x7 + x8))))))) *)
let sum : exp =
  (Add(Var "x1",
       Add(Var "x2",
           Add(Var "x3",
               Add(Var "x4",
                   Add(Var "x5",
                       Add(Var "x6",
                           Add(Var "x7",
                               Var "x8"))))))))
 

(* x1 * (x2 * (x3 * (x4  (x5 * (x6 * (x6 * (x7 * x8))))))) *)
let product : exp =
  (Mul(Var "x1",
       Mul(Var "x2",
           Mul(Var "x3",
               Mul(Var "x4",
                   Mul(Var "x5",
                       Mul(Var "x6",
                           Mul(Var "x7",
                               Var "x8"))))))))


(* x1 * x1 *)
let square : exp =
  (Mul(Var "x1", Var "x1"))


(* (2 * (x1 + x2)) *)
let perimeter : exp = Mul (Const 2L, Add (Var "x1", Var "x2"))

let select_exp = function
  | "sum" -> sum
  | "product" -> product
  | "square" -> square
  | "perimeter" -> perimeter
  | x -> failwith (Printf.sprintf "Unknown expression %s" x)

(* Sample programs -------------------------------------------------------- *)
(*
 t := x1;
 t := t + x2;
 t := t + x3;
 t := t + x4;
 t := t + x5;
 t := t + x6;
 t := t + x7;
 t := t + x8;
 return t
 *)
let sum_instrs : cmd =
  Seq (Assn ("t", Var "x1"),
       Seq (Assn ("t", Add (Var "t", Var "x2")),
            Seq (Assn ("t", Add (Var "t", Var "x3")),
                 Seq (Assn ("t", Add (Var "t", Var "x4")),
                      Seq (Assn ("t", Add (Var "t", Var "x5")),
                           Seq (Assn ("t", Add (Var "t", Var "x6")),
                                Seq (Assn ("t", Add (Var "t", Var "x7")),
                                     Seq (Assn ("t", Add (Var "t", Var "x8")),
                                          Return (Var "t")))))))))
                                          
(*
 t := x1;
 t := x1 + x2;
 return t;
 t := t + 2
 *)
let early_exit : cmd =
  Seq (Assn ("t", Var "x1"),
       Seq (Assn ("t", Add (Var "t", Var "x2")),
            Seq (Return (Var "t"),
                 Assn ("t", Mul (Var "t", Const 2L)))))
  

(*
  n := x1;
  ans := 1;
  whileLZ (-n) {
    ans := ans * n;
    n := n - 1;
  };
  return ans
 *)
let factorial : cmd =
  let n = "n" in
  let ans = "ans" in
  Seq (Seq (Assn (n, Var "x1"),
            Seq (Assn (ans, Const 1L),
                 WhileLZ(Neg (Var n),
                         Seq (Assn(ans, Mul(Var ans, Var n)),
                              Assn(n, Add(Var n, Const (-1L))))))),
       Return (Var ans))

let select_cmd = function
  | "sum_instrs" -> sum_instrs
  | "early_exit" -> early_exit
  | "factorial" -> factorial
  | x -> failwith (Printf.sprintf "Unknown command %s" x)

(* 
  Calculate the set of variables mentioned in either an expression or a command. 
*)

module OrderedVars = struct
  type t = var
  let compare = String.compare
end

module VSet = Set.Make(OrderedVars)
let (++) = VSet.union

let rec vars_of_exp (e:exp) : VSet.t =
  begin match e with
    | Var "x1" | Var "x2" | Var "x3" | Var "x4"
    | Var "x5" | Var "x6" | Var "x7" | Var "x8" ->
      VSet.empty
    | Var x -> VSet.singleton x
    | Add(e1, e2)
    | Mul(e1, e2) ->
      (vars_of_exp e1) ++ (vars_of_exp e2)
    | Neg(e) -> vars_of_exp e
    | Const _ -> VSet.empty
  end

let rec vars_of_cmd (c:cmd) : VSet.t =
  begin match c with
    | Skip -> VSet.empty
    | Assn(x, e) -> (VSet.singleton x) ++ (vars_of_exp e)
    | IfLZ(e, c1, c2) ->
      (vars_of_exp e) ++
      (vars_of_cmd c1) ++
      (vars_of_cmd c2)
    | WhileLZ(e, c) ->
      (vars_of_exp e) ++
      (vars_of_cmd c)
    | Seq(c1, c2) ->
      (vars_of_cmd c1) ++
      (vars_of_cmd c2)
    | Return e -> vars_of_exp e
  end

let var_list (c:cmd) : var list = VSet.elements (vars_of_cmd c)
