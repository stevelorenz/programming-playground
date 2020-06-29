section .data

test_string: db "abcdef", 0

section .text
global _start

strlen:
    xor rax, rax
; main loop starts here
.loop:
    cmp byte [rdi+rax], 0  ; check if the current byte is the null-terminator
    je .end  ;  jump if we found null-terminator
    inc rax  ; Otherwise continue the loop and increase the counter.
    jmp .loop

.end:
    ret  ; when we hit 'ret', rax should hold return value.

_start:
    mov rdi, test_string
    call strlen
    mov rdi, rax  ; the result is in rax, so then passed as the exit code of the exit syscall

    mov rax, 60
    syscall  ; Use echo $? in the shell to check the result.
