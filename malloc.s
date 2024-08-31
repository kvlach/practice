section .data
	str_success db "success", 0xA
	str_failure db "failure", 0xA
	one_two_three db "123"

section .text
	global _start

_start:
	mov rax, 9    ; syscall = mmap
	mov rdi, 0    ; void *addr
	mov rsi, 1024 ; size_t len
	mov rdx, 3    ; int prot = PROT_READ | PROT_WRITE
	mov r10, 10   ; int flags = MAP_PRIVATE | MAP_ANONYMOUS
	mov r8,  -1   ; int fd
	mov r9,  0    ; off_t off
	syscall

	cmp rax, -1
	je error

	mov r15, rax

	mov rax, 1           ; syscall = write
	mov rdi, 1           ; int fd = stdout
	mov rsi, str_success ; const void *buf
	mov rdx, 8           ; size_t nbytes
	syscall

	mov r14, [r15]

	mov dword [r15], 123
	; mov rax, 1
	; mov rdi, 1
	; mov rsi, [r15]
	; mov rdx, 3
	; syscall

	mov rax, 60 ; syscall = exit
	mov rdi, 0  ; int status
	syscall

error:
	mov rax, 1           ; syscall = write
	mov rdi, 1           ; int fd = stdout
	mov rsi, str_failure ; const void *buf
	mov rdx, 8           ; size_t nbyte
	syscall

	mov rax, 60 ; syscall = exit
	mov rdi, 1  ; int status
	syscall
