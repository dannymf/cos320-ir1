(* Syntax-directed translation *)

open Ast (* Source language *)
open X86 (* Target language *)


(* Assumption: expressions may refer to the variables x1, ..., x8.  We
   want to compile an expression to a procedure that takes x1, ..., x8
   as input and returns the value of the expression. *) 

(* x86_64 System V AMD64 ABI calling convention: 
   - first six integer/pointer args are passed in: rdi, rsi, rdx, rcx, r8, r9
   - arguments seven and higher are pushed onto the stack (right-to-left)  *)
let compile_var (x:var) : X86.operand =
  Asm.(match x with
      | "x1" -> ~%Rdi
      | "x2" -> ~%Rsi
      | "x3" -> ~%Rdx
      | "x4" -> ~%Rcx
      | "x5" -> ~%R08
      | "x6" -> ~%R09
      | "x7" -> Ind3 (Lit 16L, Rbp)
      | "x8" -> Ind3 (Lit 24L, Rbp)
      | _ -> failwith (Printf.sprintf "Invalid variable name %s" x))

(* Compilation strategy:
    - An expression is compiled to a list of instructions that store
      the value of the expression in rax
    - use the stack to remember intermediate results for binary operations
    - use the dedicated register R10 for binary operations. *)
let rec compile_exp (e:exp) : X86.ins list =
  let compile_bop op e1 e2 =
     compile_exp e1
    @ Asm.[Pushq, [~%Rax]]
    @ compile_exp e2
    @ Asm.[Popq, [~%R10];
           op, [~%R10; ~%Rax]]
  in
  match e with
  | Var v -> Asm.[Movq, [compile_var v; ~%Rax]]
  | Const n -> Asm.[Movq, [Imm (Lit n); ~%Rax]]
  | Neg e1 -> compile_exp e1 @ Asm.[Negq, [~%Rax]]
  | Add (e1, e2) -> compile_bop Addq e1 e2
  | Mul (e1, e2) -> compile_bop Imulq e1 e2


let compile (src:exp) : X86.prog =
  let function_prologue : X86.ins list =
    Asm.([ Pushq, [~%Rbp]          (* save frame pointer *)
         ; Movq, [~%Rsp; ~%Rbp]    (* create new frame pointer *)
         ])
  in
  let function_epilogue : X86.ins list =
    Asm.([ Popq, [~%Rbp]           (* restore frame pointer *)
         ; Retq, []                (* return to caller *)
         ])
  in
  Asm.[gtext "program" @@
       function_prologue 
       @ (compile_exp src)
       @ function_epilogue
      ]

let () =
  let src = select_exp Sys.argv.(1) in
  let tgt = compile src in
  print_endline (X86.string_of_prog tgt)
