	.text
	.globl	program
program:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, %rax
	pushq	%rax
	movq	%rsi, %rax
	pushq	%rax
	movq	%rdx, %rax
	pushq	%rax
	movq	%rcx, %rax
	pushq	%rax
	movq	%r8 , %rax
	pushq	%rax
	movq	%r9 , %rax
	pushq	%rax
	movq	16(%rbp), %rax
	pushq	%rax
	movq	24(%rbp), %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%r10
	addq	%r10, %rax
	popq	%rbp
	retq	
