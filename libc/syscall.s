bits 64

global __syscall

section .text
__syscall:
	push rbp
	mov rbp, rsp

	; Callee-save registers
	push rbx
	push r12
	push r13
	push r14
	push r15

	; The syscall calling convention on x64 corresponds to the x64 calling
	; convention, with syscall number in rax. Since we pass the syscall number
	; as the first argument and then the rest, we shift them all down into rax.
	mov rax, rdi
	mov rdi, rsi
	mov rsi, rdx
	mov rdx, rcx

	int 0x80

	pop r15
	pop r14
	pop r13
	pop r12
	pop rbx

	pop rbp
	ret
