bits 64

global __syscall

section .text
__syscall:
	push rbp
	mov rbp, rsp

	; Regular callee-save registers
	push rbx
	push r12
	push r13
	push r14
	push r15

	; The kernel also clobbers rcx and r11
	push rcx
	push r11

	; The syscall calling convention on x64 is to pass the syscall number in
	; rax, and all arguments in registers in the following sequence:
	;   rdi, rsi, rdx, r10, r8, r9
	; This is almost the same as the regular System V x86-64 calling
	; convention, except argument 4 is in r10 rather than rcx.
	mov rax, rdi
	mov rdi, rsi
	mov rsi, rdx
	mov rdx, rcx
	mov r10, r8
	mov r8, r9
	mov r9, [rbp + 16]

	syscall

	pop r11
	pop rcx

	pop r15
	pop r14
	pop r13
	pop r12
	pop rbx

	pop rbp
	ret
