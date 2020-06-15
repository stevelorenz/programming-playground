section .data
codes:
    db '0123456789ABCDEF'

section .text
global _start

_start:
    mov rax, 0x1122334455667788
    mov rdi, 1
    mov rdx, 1
    mov rcx, 64

.loop:
    push rax
    sub rcx, 4
    sar rax, cl
    and rax, 0xf

    lea rsi, [codes+rax]
    mov rax, 1
    push rcx  ; syscall can change rcx.
    syscall
    pop rcx

    pop rax
    ; test can be used to fast test if it is zero
    test rcx, rcx
    jnz .loop

    mov rax, 60
    xor rdi, rdi
    syscall
