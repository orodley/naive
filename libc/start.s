bits 64

global _start
extern main

section .text
_start:
	xor ebp, ebp
	call main
	mov bl, al
	mov eax, 1
	int 0x80
