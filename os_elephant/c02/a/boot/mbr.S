; MBR
;------------------------------------------------------------
SECTION MBR vstart=0x7c00 ; The start address of MBR is 0x7c00
   mov ax,cs
   mov ds,ax
   mov es,ax
   mov ss,ax
   mov fs,ax
   mov sp,0x7c00

; Clean screen and print the test string
; -----------------------------------------------------------
   mov ax, 0x600
   mov bx, 0x700
   mov cx, 0 ; Top left corner: (0, 0)
   mov dx, 0x184f; Top right corner: (80,25)
   int 0x10 ; Invoke 0x10 BIOS interrupt

   mov ah, 3
   mov bh, 0
   int 0x10

   mov ax, message ; Store to-be-printed string in ax
   mov bp, ax

   mov cx, 9 ; String length
   mov ax, 0x1301
   mov bx, 0x2
   int 0x10

   jmp $ ; Loop here

   message db "1 MBR(ZZ)"
   times 510-($-$$) db 0
   db 0x55,0xaa
