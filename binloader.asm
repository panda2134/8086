.686
.model flat, stdcall
option casemap:none

include windows.inc
include kernel32.inc
include user32.inc

includelib user32.lib
includelib kernel32.lib
includelib msvcrt.lib
printf          PROTO C :ptr byte, :VARARG

.data
    filename byte "./test_bin/c.img", 0
.data?
    memory byte 1048576 dup(?)
.code

; we assume that the memoryory is of 1MB size, and code should be loaded at 7c0:0
; that is different from real 8086s which boot up from ffff:0
; however 7c0:0 is more convenient for our test binary
; time permitted, we might switch to ffff:0 and code a small bios

LoadBinaryIntoEmulator PROC USES ebx esi, mem:ptr byte, lpFileName:ptr byte
LOCAL hFile:dword, numBytesRead:dword, fileSize:dword, mbrAddr:ptr dword

    MBR_SIZE equ 512
    mov numBytesRead, 0
    INVOKE CreateFileA, lpFileName, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    cmp eax, INVALID_HANDLE_VALUE
    je loadbin_error_ret
    mov hFile, eax
    INVOKE GetFileSize, hFile, 0
    cmp eax, 512
    jb loadbin_file_small
    mov eax, 512
loadbin_file_small:
    mov fileSize, eax
    mov ecx, dword ptr [mem]
    add ecx, 7c00h
    mov mbrAddr, ecx
    INVOKE ReadFile, hFile, ecx, fileSize, ADDR numBytesRead, 0 ; read file to 7c00h
    test eax, eax
    jz loadbin_error_ret
    mov ecx, [mbrAddr]
    mov eax, [ecx + 510]
    cmp eax, 0aa55h ; check for mbr flag
    jne loadbin_error_ret ; error if no flag detected
    mov eax, 0 ; success
    ret
loadbin_error_ret:
    mov eax, 1
    ret
LoadBinaryIntoEmulator ENDP

_start PROC
    INVOKE LoadBinaryIntoEmulator, ADDR memory, ADDR filename
_cleanup:
    mov eax, 0
    ret
_start ENDP
END _start