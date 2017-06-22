bits 64

global _start
extern main
extern exit

section .text
_start:
	xor ebp, ebp
	pop rdi
	mov rsi, rsp
	sub rsp, 8
	call main
	mov edi, eax
	call exit
