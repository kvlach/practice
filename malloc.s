section .data
	success db "success"
	failed db "failed"

section .text
	global _start

_start:
	mov rax, 9    ; mmap
	mov rdi, 0    ; void *addr
	mov rsi, 1024 ; size_t len
	mov rdx, 3    ; int prot = PROT_READ | PROT_WRITE
	mov r10, 2    ; int flags = MAP_PRIVATE
	mov r8,  -1   ; int fildes
	mov r9,  0    ; off_t off
	syscall

	cmp rax, -1   ; if -1, failed
	je error

done:
	mov rax, 1 ; write
	mov rdi, 1 ; stdout
	mov rsi, success
	mov rdx, 7 ; len
	syscall

	mov rax, 60 ; exit
	mov rdi, 0  ; int status
	syscall

error:
	mov rax, 1 ; write
	mov rdi, 1 ; stdout
	mov rsi, failed
	mov rdx, 6 ; len
	syscall

	mov rax, 60 ; exit
	mov rdi, 1  ; int status
	syscall
