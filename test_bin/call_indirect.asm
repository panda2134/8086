org 0x7c00
bits 16

nop

mov ax, 0
mov ds, ax
mov ss, ax
mov es, ax
mov bp, 7c0h
mov sp, 7c00h

mov bx, 0
mov di, 0
call far [di+3ff0h]
