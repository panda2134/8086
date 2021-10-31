org 0x7c00
bits 16

mov ax, 0
mov ds, ax
mov ss, ax
mov es, ax
mov bp, 8000h
mov sp, 8000h

mov ax, 1
mov dx, 2
xchg ax, dx
cmp ax, 2
hlt

times 512-2-($-$$) db 0
db 0x55
db 0xaa
