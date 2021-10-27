#!/bin/sh

nasm test.asm -f bin -o test.bin
if [[ $? -ne 0 ]]; then
    exit
fi
dd conv=notrunc if=test.bin of=c.img bs=512 count=1
qemu-system-x86_64 -drive format=raw,file=c.img