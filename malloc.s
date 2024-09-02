section .data
	success db "success", 0xA
	success_len equ $ - success
	failure db "failure", 0xA
	failure_len equ $ - failure

	message db "Hello World!", 0xA, 0x0

section .text
	global _start

_start:
	mov rax, 9     ; syscall = mmap
	mov rdi, 0     ; void *addr
	mov rsi, 1024  ; size_t len
	mov rdx, 3     ; int prot = PROT_READ | PROT_WRITE
	mov r10, 34    ; int flags = MAP_PRIVATE | MAP_ANON
	mov r8,  -1    ; int fd (since MAP_ANON)
	mov r9,  0     ; off_t off
	syscall

	cmp rax, -1
	je error

	mov r13, rax

	mov rax, 1            ; syscall = write
	mov rdi, 1            ; int fd = stdout
	mov rsi, success      ; void *buf
	mov rdx, success_len  ; size_t nbyte
	syscall

	mov r14, message
	mov r15, 0

	.loop:
	mov al, [r14+r15] ; al to copy one byte at a time
	cmp al, 0
	je .end
	mov [r13+r15], al
	inc r15
	jmp .loop
	.end:

	mov rax, 1    ; syscall = write
	mov rdi, 1    ; int fd = stdout
	mov rsi, r13  ; void *buf
	mov rdx, r15  ; size_t nbyte
	syscall	

	mov rax, 11    ; syscall = munmap
	mov rdi, r13   ; void *addr
	mov rsi, 1024  ; size_t len
	syscall

	cmp rax, -1
	je error

	mov rax, 1            ; syscall = write
	mov rdi, 1            ; int fd = stdout
	mov rsi, success      ; void *buf
	mov rdx, success_len  ; size_t nbyte
	syscall

	mov rax, 60  ; syscall exit
	mov rdi, 0   ; int status
	syscall

error:
	mov rax, 1            ; syscall = write
	mov rdi, 1            ; int fd = stdout
	mov rsi, failure      ; void *buf
	mov rdx, failure_len  ; size_t nbyte
	syscall

	mov rax, 60  ; syscall exit
	mov rdi, 1   ; int status
	syscall
