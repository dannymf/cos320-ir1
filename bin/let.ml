open X86

type var = Ast.var

(* simple let language intermediate representation -------------------------- *)
module IR = struct
  (* Unique identifiers for temporaries. *)
  type uid = int

  (* syntactic values *)
  type opn = 
    | Id of uid
    | Const of int64
    | Var of var

  (* binary operations *)
  type bop =
    | Add
    | Mul
             
  (* instructions *)
  (* note that there is no nesting of operations! *)
  type instr =
    | Let of uid * bop * opn * opn

  type program = {
    instrs: instr list;
    ret: opn
  }

  type ctx =
    { mutable max_uid : int }

  (* How many unique identifiers have been allocated *)
  let nb_uid c = c.max_uid

  (* Create a new context *)
  let mk_context : unit -> ctx =
    fun () -> { max_uid = 0 }
  
  (* "gensym" -- generate a new unique identifier *)
  let mk_uid (c:ctx) : uid = 
    let uid = c.max_uid in
    c.max_uid <- c.max_uid + 1;
    uid

  (* Pretty printing *)
  let pp_uid u = Printf.sprintf "tmp%d" u
  let pp_var x = Printf.sprintf "var%s" x
      
  let pp_opn = function 
    | Id u   -> pp_uid u
    | Const c -> (Int64.to_string c)^"L"
    | Var x   -> pp_var x

  let pp_bop = function
    | Add -> "add"
    | Mul -> "mul"

  let pp_instr = function
    | Let (u, bop, op1, op2) ->
      Printf.sprintf "let %s = %s %s %s"
        (pp_uid u) (pp_bop bop) (pp_opn op1) (pp_opn op2)

  let pp_program {instrs; ret} =
    (String.concat " in\n" (List.map pp_instr instrs)) ^
    (Printf.sprintf " in\n  ret %s" (pp_opn ret))
end


(* SIMPLE -> let translation -------------------------------------------------------- *)
(* Given a context c and expression e represented as an Ast, generate
   a pair (instrs, opn) such that after executing the sequence instr,
   opn evaluates to the same value as e *)
let rec translate_exp (c : IR.ctx) (e : Ast.exp) : IR.program =
  let translate_bop op e1 e2 =
     let p1 = translate_exp c e1 in
     let p2 = translate_exp c e2 in
     let id = IR.mk_uid c in
     IR.{ instrs = p1.instrs@p2.instrs@[Let (id, op, p1.ret, p2.ret)]; ret = Id id }
  in
  match e with
  | Var v -> IR.{ instrs = []; ret = Var v }
  | Const k -> IR.{ instrs = []; ret = Const k }
  | Add (e1, e2) -> translate_bop IR.Add e1 e2
  | Mul (e1, e2) -> translate_bop IR.Mul e1 e2
  | Neg e1 -> translate_bop IR.Mul e1 (Const (-1L))


(* Let -> x86 code generation ----------------------------------------------------- *)

(* Strategy:
   - { instrs; ret } is compiled to a list of instructions that executes instrs, and then
     stores the value of ret in rax
   - Each uid is assigned a stack slot -- uid n is stored at Rbp - 8*(n+1).
   - use the dedicated register R10 for binary operations.
*)

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
  | Var "x1" -> ~%Rdi
  | Var "x2" -> ~%Rsi
  | Var "x3" -> ~%Rdx
  | Var "x4" -> ~%Rcx
  | Var "x5" -> ~%R08
  | Var "x6" -> ~%R09
  | Var "x7" -> Ind3 (Lit 16L, Rbp)
  | Var "x8" -> Ind3 (Lit 24L, Rbp)
  | Var x -> failwith (Printf.sprintf "Invalid variable name %s" x)

let codegen_instr : IR.instr -> X86.ins list =
  function | Let (id, op, x, y) ->
              let opcode = match op with
                | IR.Add -> Addq
                | IR.Mul -> Imulq
              in
              Asm.[Movq, [codegen_operand x; ~%Rax];
                   Movq, [codegen_operand y; ~%R10];
                   opcode, [~%R10; ~%Rax];
                   Movq, [~%Rax; codegen_uid id]]
  
let codegen (c : IR.ctx) (p : IR.program) : X86.ins list =
  let prologue =
    Asm.[Pushq, [~%Rbp]                        (* save frame pointer *)
        ; Movq, [~%Rsp; ~%Rbp]                 (* create new frame pointer *)
        ; Subq, [~$((IR.nb_uid c) * 8); ~%Rsp] (* allocate local storage *)
        ]
  in
  let body =
    (List.flatten (List.map codegen_instr p.IR.instrs))
    @ Asm.[Movq, [codegen_operand p.IR.ret; ~%Rax]]
  in
  let epilogue =
    Asm.[Addq, [~$((IR.nb_uid c) * 8); ~%Rsp] (* deallocate local storage *)
        ;Popq, [~%Rbp]                        (* restore frame pointer *)
        ;Retq, []]                            (* return control to caller *)
  in  
  prologue @ body @ epilogue

let compile (ast : Ast.exp) : X86.prog =
  let c = IR.mk_context () in
  let ir = translate_exp c ast in
  Asm.[ gtext "program" (codegen c ir) ]

let () =
  let src = Ast.select_exp Sys.argv.(1) in
  let tgt = compile src in
  print_endline (X86.string_of_prog tgt)
