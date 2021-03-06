IFNDEF TERM_INC
TERM_INC equ <1>

InitEmuScreen PROC ; call this only once to initialize the emulator screen
LOCAL hCon:dword, hWin:dword, termSize:COORD, rect:SMALL_RECT, cursorInfo:CONSOLE_CURSOR_INFO
    INVOKE GetStdHandle, STD_OUTPUT_HANDLE
    mov hCon, eax
    mov rect.Left, 0
    mov rect.Top, 0
    mov rect.Right, 80-1
    mov rect.Bottom, 28
    INVOKE SetConsoleWindowInfo, hCon, 1, ADDR rect ; set the size of display area
    mov termSize.x, 80
    mov termSize.y, 29                              ; 25 lines for output, 2 lines for status, 1 line empty
    INVOKE SetConsoleScreenBufferSize, hCon, dword ptr termSize ; set the size of buffer (1 extra line)
    INVOKE GetConsoleWindow
	mov hWin, eax
	INVOKE GetWindowLong, hWin, GWL_STYLE
	mov ecx, WS_MAXIMIZEBOX
	not ecx
	and eax, ecx
    mov ecx, WS_MINIMIZEBOX
    not ecx
    and eax, ecx
	mov ecx, WS_SIZEBOX
	not ecx
	and eax, ecx ; eax := eax & ~WS_MAXIMIZEBOX & ~WS_SIZEBOX & ~WS_MINIMIZEBOX
	INVOKE SetWindowLong, hWin, GWL_STYLE, eax
    mov [cursorInfo.dwSize], 1
    mov [cursorInfo.bVisible], 0
    INVOKE SetConsoleCursorInfo, hCon, ADDR cursorInfo
	ret
InitEmuScreen ENDP

WriteEmuScreen PROC USES ebx esi, mem:ptr byte ; update the emulator screen with memory starting from b800:0
LOCAL hCon:dword, termCoord:COORD, textAttr:dword

    count equ 80*25
    mov esi, mem
    INVOKE GetStdHandle, STD_OUTPUT_HANDLE
    mov hCon, eax
    mov ecx, 0
_loop:
    cmp ecx, count
    jge _loop_end
    mov edx, ecx
    shr edx, 16 
    movzx eax, cx ; prepare ecx in dx:ax
    mov bx, 80   ; divide by 80 to get num of lines
    div bx
    mov [termCoord.y], ax
    mov [termCoord.x], dx
    push ecx
    INVOKE SetConsoleCursorPosition, hCon, dword ptr termCoord ; move cursor to termCoord
	movzx ecx, byte ptr [esi+1]
	mov textAttr, ecx
    INVOKE SetConsoleTextAttribute, hCon, textAttr ; set color attrs, etc.
    INVOKE WriteConsoleA, hCon, esi, 1, 0, 0 ; put one char
    pop ecx
    add esi, 2
    inc ecx
    jmp _loop
_loop_end:
    INVOKE SetConsoleCursorPosition, hCon, 0
    INVOKE SetConsoleTextAttribute, hCon, 0 ; not visible
    mov eax, 0
    ret
WriteEmuScreen ENDP

loadStatusColor MACRO colorType, dest
    IFIDN <colorType>, <running>
        mov dest, BACKGROUND_GREEN or BACKGROUND_INTENSITY
        EXITM
    ENDIF
    IFIDN <colorType>, <paused>
        mov dest, BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY
        EXITM
    ENDIF
    echo Error: Unknown status
ENDM

WriteStatusLine PROC statusLine1:ptr byte, statusLine2:ptr byte, textAttr:dword ; suppose that both lines are 80 chars
LOCAL hCon:dword, termCoord:COORD

    INVOKE GetStdHandle, STD_OUTPUT_HANDLE
    mov hCon, eax
    INVOKE SetConsoleTextAttribute, hCon, textAttr
    mov [termCoord.x], 0
    mov [termCoord.y], 26 ; second last line
    
    INVOKE SetConsoleCursorPosition, hCon, dword ptr termCoord
    INVOKE WriteConsoleA, hCon, statusLine1, 80, 0, 0
    add [termCoord.y], 1
    INVOKE SetConsoleCursorPosition, hCon, dword ptr termCoord
    INVOKE WriteConsoleA, hCon, statusLine2, 80, 0, 0

    INVOKE SetConsoleCursorPosition, hCon, 0
    INVOKE SetConsoleTextAttribute, hCon, 0 ; not visible
    ret

WriteStatusLine ENDP

; GenTest PROC USES ebx esi edi, mem:ptr byte ; generate example text
; LOCAL chr:byte, startChr:byte
;     count equ 80*25
; 	mov esi, mem
; 	mov ecx, 0
;     mov [startChr], 2fh
; _loop_gen:
;     cmp ecx, count
;     jae _loop_gen_end
;     mov edx, ecx
;     shr edx, 16 
;     movzx eax, cx ; prepare ecx in dx:ax
;     mov bx, 80   ; divide by 80 to get num of lines
;     div bx
;     cmp dx, 0
;     jne _not_first_col
;     mov al, [startChr]
; 	inc al
; 	cmp al, '9'
; 	jbe _L0
; 	mov al, '0'
; _L0:
; 	mov [startChr], al
;     mov [chr], al
; _not_first_col:
; 	mov al, [chr]
; 	mov byte ptr [esi], al
;     inc al
;     cmp al, '9'
;     jbe _L1
;     mov al, '0'
; _L1:
; 	mov [chr], al
; 	mov byte ptr [esi+1], 10
; 	add esi, 2
;     inc ecx
; 	jmp _loop_gen
; _loop_gen_end:
;     ret
; GenTest ENDP

; _start PROC
;     INVOKE InitEmuScreen
;     INVOKE GenTest, ADDR buf
;     INVOKE WriteEmuScreen, ADDR buf
;     INVOKE Sleep, 5000
; _cleanup:
;     mov eax, 0
;     ret
; _start ENDP
; END _start
ENDIF