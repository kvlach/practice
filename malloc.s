section .data
	success db "success", 0xA
	success_len equ $ - success
	failure db "failrue", 0xA
	failure_len equ $ - failure

	message db "Hello World!", 0xA, 0x0
	; message_len equ $ - message

section .text
	global _start

_start:
	mov rax, 9
	mov rdi, 0
	mov rsi, 1024
	mov rdx, 3
	mov r10, 34
	mov r8,  -1
	mov r9,  0
	syscall

	cmp rax, -1
	je error

	mov r15, rax

	mov rax, 1
	mov rdi, 1
	mov rsi, success
	mov rdx, success_len
	syscall

	; mov [r15], byte message

	mov r13, message
	mov r14, 0

	.loop:
	mov r12, [r13+r14]
	mov [r15+r14], r12
	cmp [r13+r14], byte 0
	je .end
	inc r14
	jmp .loop

	.end:

	mov rax, 1
	mov rdi, 1
	mov rsi, r15
	mov rdx, r14
	syscall

	mov rax, 60
	mov rdi, 0
	syscall

error:
	mov rax, 1
	mov rdi, 1
	mov rsi, failure
	mov rdx, failure_len
	syscall

	mov rax, 60
	mov rdi, 1
	syscall
