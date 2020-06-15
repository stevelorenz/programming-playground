section .data

message: db "hello, world!", 10

section .bss

section .text
global _start

_start:
    mov rax, 1  ; system call number stored in rax
    mov rdi, 1  ; file descriptor
    mov rsi, message
    mov rdx, 14  ; the length of the string.
    syscall  ; invoke the system call.

    mov rax, 60  ; 'exit' syscall number
    xor rdi, rdi  ; set rdi to zero
    syscall;
