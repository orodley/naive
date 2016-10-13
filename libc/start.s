bits 64

global _start
extern main
extern exit

section .text
_start:
	xor ebp, ebp
	call main
	mov edi, eax
	call exit
