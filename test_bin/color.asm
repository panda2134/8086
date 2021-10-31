org 0x7c00
bits 16

mov ax, 0xb800
mov ds, ax
mov ax, 0
mov ss, ax
mov es, ax
mov bp, 8000h
mov sp, 8000h

mov cx, 4000
mov di, 0

loop:

    mov byte [di], ' '
    or al, 10000000b
    mov [di+1], al

add al, 00010000b
add di, 2
dec cx
jnz loop

hlt

times 512-2-($-$$) db 0
db 0x55
db 0xaa