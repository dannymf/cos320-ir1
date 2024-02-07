open X86

type var = Ast.var

(*
  Let's extend our compiler to translate a subset of SIMPLE commands:
    <cmd> ::= 
         |  skip   (do nothing)
         |  <var> = <exp>
         |  <cmd>; <cmd>
         |  return <exp>

  (That is, omitting the control flow commands
         |  ifLZ <exp> { <cmd> } else { <cmd> }
         |  whileLZ <exp> { <cmd> })
 *)

(* simple let language intermediate representation -------------------------- *)
module IR = struct
  (* Unique identifiers for temporaries. *)
  type uid = int

  (* A context maintains information for translating to/from the IR.
     In particular, used to allocate UIDs when translating to the IR
     and to manage stack layout when generating X86. *)
  type ctx =
    { mutable max_uid : int; (* number of allocated temporaries *)
      vars : var list (* list of (non-temporary, non-parameter) variables *)
    }

  (* Number of allocated temporaries *)
  let nb_uid c = c.max_uid

  (* Number of (non-temporary, non-parameter) variables *)
  let nb_vars c = List.length c.vars

  (* Allocate a new context *)
  let mk_context : var list -> ctx =
    fun vars -> { max_uid = 0; vars = vars }
  
  (* Find the numerical position of a variable in a context *)
  let var_position ctx var =
    let rec go n xs =
      match xs with
      | (x::xs) ->
        if x = var then n else go (n + 1) xs
      | [] -> failwith (Printf.sprintf "Variable %s is not known to contex" var)
    in
    go 0 ctx.vars

  (* "gensym" -- generate a new unique identifier *)
  let mk_uid (c:ctx) : uid = 
    let uid = c.max_uid in
    c.max_uid <- c.max_uid + 1;
    uid

  (* operands *)
  type opn = 
    | Id of uid
    | Const of int64

  (* binary operations *)
  type bop =
    | Add
    | Mul
  
  (* instructions *)
  (* note that there is no nesting of operations! *)
  type instr =
    | Let of uid * bop * opn * opn
    | Load of uid * var
    | Store of var * opn
    | Return of opn
  
  type program = {
    instrs: instr list;
    ret: opn
  }

  (* pretty printing *)
  let pp_uid u = Printf.sprintf "tmp%d" u
  let pp_var x = Printf.sprintf "var%s" x
  
  let pp_opn = function 
    | Id u   -> pp_uid u
    | Const c -> (Int64.to_string c)^"L"

  let pp_bop = function
    | Add -> "add"
    | Mul -> "mul"
  
  let pp_instr = function
    | Let (u, bop, op1, op2) ->
      Printf.sprintf "%s := %s %s %s"
        (pp_uid u) (pp_bop bop) (pp_opn op1) (pp_opn op2)
    | Load (u, x) ->
      Printf.sprintf "%s := load %s"
        (pp_uid u) (pp_var x)
    | Store (x, op) ->
      Printf.sprintf "store %s := %s"
        (pp_var x) (pp_opn op)
    | Return op ->
      pp_opn op
  
  let pp_program instrs =
    (String.concat "\n" (List.map pp_instr instrs))
end


(* Given a context c and expression e represented as an Ast, generate
   a pair (instrs, opn) such that after executing the sequence instr,
   opn evaluates to the same value as e *)
let rec translate_exp (c : IR.ctx) (e : Ast.exp) : IR.program =
  let translate_bop bop e1 e2 =
    let p1 = translate_exp c e1 in
    let p2 = translate_exp c e2 in
    let id = IR.mk_uid c in
    IR.{ instrs = p1.instrs@p2.instrs@[Let (id, bop, p1.ret, p2.ret)];
         ret = Id id }
  in
  match e with
  | Var v ->
     let id = IR.mk_uid c in
     IR. { instrs = [Load (id, v)]; ret=Id id }
  | Const k -> IR.{ instrs = []; ret = Const k }
  | Add (e1, e2) -> translate_bop IR.Add e1 e2
  | Mul (e1, e2) -> translate_bop IR.Mul e1 e2
  | Neg e -> translate_bop IR.Mul (Ast.Const (-1L)) e

let rec translate_cmd (ctx : IR.ctx) (cmd : Ast.cmd) : IR.instr list =
  match cmd with
  | Skip -> []
  | Assn (lhs, rhs) ->
     let p = translate_exp ctx rhs in
     p.instrs@IR.[Store (lhs, p.ret)]
  | Seq (c1, c2) -> (translate_cmd ctx c1)@(translate_cmd ctx c2)
  | Return exp ->
     let p = translate_exp ctx exp in
     p.instrs@IR.[Return p.ret]
  | IfLZ (_, _, _) | WhileLZ (_, _) -> failwith "Not implemented"


(* Translate uid to indirect address *)
let codegen_uid uid =
  Ind3 (Lit (Int64.of_int ((-8) * (uid + 1))), Rbp)

(* Translate IR operand to X86 operand *)
let codegen_operand (operand : IR.opn) : X86.operand =
  let open Asm in
  let open IR in
  match operand with
  | Id uid -> codegen_uid uid
  | Const k -> Imm (Lit k)

(* Translate variable to X86 operand *)
let codegen_variable (c : IR.ctx) (v : var) : X86.operand =
  let open Asm in
  match v with
  | "x1" -> ~%Rdi
  | "x2" -> ~%Rsi
  | "x3" -> ~%Rdx
  | "x4" -> ~%Rcx
  | "x5" -> ~%R08
  | "x6" -> ~%R09
  | "x7" -> Ind3 (Lit 16L, Rbp)
  | "x8" -> Ind3 (Lit 24L, Rbp)
  | _ ->
     (* Variables stored *below* (lower addresses) uids in the stack *)
    let pos = IR.var_position c v in
    let disp = Int64.mul (-8L) (Int64.of_int (1 + pos + (IR.nb_uid c))) in
    Ind3 (Lit disp, Rbp)


let codegen_instr (c : IR.ctx) (ins : IR.instr) : X86.ins list =
  match ins with
  | (IR.Let (uid, op, x, y)) ->
    let opcode = match op with
      | IR.Add -> Addq
      | IR.Mul -> Imulq
    in
    Asm.[Movq, [codegen_operand x; ~%Rax]
        ;Movq, [codegen_operand y; ~%R10]
        ;opcode, [~%R10; ~%Rax]
        ;Movq, [~%Rax; codegen_uid uid]]
  | (IR.Load (uid, v)) ->
     Asm.[Movq, [codegen_variable c v; ~%Rax]
         ;Movq, [~%Rax;  codegen_uid uid]]
  | (IR.Store (v, opn)) ->
     Asm.[Movq, [codegen_operand opn; ~%Rax]
         ;Movq, [~%Rax; codegen_variable c v]]
  | (IR.Return opn) ->
     Asm.[Movq, [codegen_operand opn; ~%Rax];
          Jmp, [Imm (Lbl "exit")]]

let codegen (c : IR.ctx) (instrs : IR.instr list) =
  let rec push_zeros n =
    if n == 0 then []
    else Asm.((Pushq, [~$0])::(push_zeros (n - 1)))
  in
  let prologue =
    Asm.[Pushq, [~%Rbp]                       (* save frame pointer *)
        ;Movq, [~%Rsp; ~%Rbp]                 (* create new frame pointer *)
        ;Subq, [~$((IR.nb_uid c) * 8); ~%Rsp] (* allocate storage for temporaries *)
        ] @ (push_zeros (IR.nb_vars c))
  in
  let body =
    List.flatten (List.map (codegen_instr c) instrs)
    @ Asm.[Movq, [~$0; ~%Rax]]
  in
  let epilogue =
    Asm.[(* deallocate local storage *)
         Addq, [~$((IR.nb_uid c + IR.nb_vars c) * 8); ~%Rsp]
        ;Popq, [~%Rbp]                        (* restore frame pointer *)
        ;Retq, []]                            (* return control to caller *)
  in
  Asm.[gtext "program" (prologue @ body)
      ; text "exit" epilogue]

let compile (ast : Ast.cmd) : X86.prog =
  let vars = Ast.var_list ast in
  let c = IR.mk_context vars in
  let ir = translate_cmd c ast in
  codegen c ir

let () =
  let src = Ast.select_cmd Sys.argv.(1) in
  let tgt = compile src in
  print_endline (X86.string_of_prog tgt)
