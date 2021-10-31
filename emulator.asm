.686
.model flat, stdcall
option casemap:none

include         windows.inc
include         kernel32.inc
include         user32.inc

includelib      user32.lib
includelib      kernel32.lib
includelib      msvcrt.lib

printf          PROTO C :ptr byte, :VARARG

.data
floppyPath      byte "./test_bin/xchg.bin", 0
haltMsgTitle    byte "Halted", 0
haltMsg         byte "HLT is executed; since interrupt is not supported, the emulator will now exit.", 0
UDMsgTitle      byte "Undefined Instruction", 0
UDMsg           byte "Encountered an undefined instruction. (#UD)", 0
debugMsg        byte "%d %d %d %d", 0AH, 0DH, 0
invalidOpMsg    byte "Invalid Operation!", 0Dh, 0Ah, 0
REGB            label byte
REGW            label word
R_AL            label byte
R_AX            word 0
R_CX            word 0
R_DX            word 0
R_BX            word 0
R_SP            word 0
R_BP            word 0
R_SI            word 0
R_DI            word 0

REGS            label word                
R_ES            word 0
R_CS            word 0 ; mbr
R_SS            word 0
R_DS            word 0

R_FLAGS         byte 0

R_IP            word 7c00H

MEMO_Guard      byte 00FFH

.data?
MEMO            byte 1048576 DUP(?)

.code


; mod[2] xxx r/m[3] passed by ah, start of instruction(host) passed by ebx
; when r/m is reg, need to pass word/byte in lowest bit of al
; not modify al, ah and ebx
; effective address(host) returned by edx,
; end of displacement field(host) returned by esi
computeEffectiveAddress MACRO LeaveLabel, DisableFallThroughLeave, SegmentType
                ; MACRO local label
                LOCAL NoDisplacement, MOD123, MOD23, RM_Decode, RM_Is1XX, RM_Is11X, AddDisplacment, MOD3, RM_IsWordReg
                ; ah already = mod[2] reg[3] r/m[3] or mod[2] op[3] r/m[3]
                mov cl, ah
                shr cl, 6; mod
                jnz MOD123
                ; mod = 00
                mov cl, ah ; mod[2] reg[3] r/m[3]
                and cl, 0111b ; r/m[3]
                cmp cl, 0110b ; check special case
                jne NoDisplacement
                ; r/m = 110 special case, 16bit displacement only
                movzx edi, word ptr [ebx + 2]
                lea esi, [ebx + 4] ; end of displacement field(host)
                xor edx, edx ; clear edx for displacement only
                jmp AddDisplacment
    NoDisplacement:
                xor edi, edi ; common case, no displacement
                lea esi, [ebx + 2] ; end of displacement field(host)
                jmp RM_Decode
    MOD123:
                cmp cl, 1
                jne MOD23
                ; mod = 01
                movzx edi, byte ptr [ebx + 2] ; 8bit displacement
                lea esi, [ebx + 3] ; end of displacement field(host)
                jmp RM_Decode
    MOD23:
                cmp cl, 2
                jne MOD3
                ; mod = 10
                movzx edi, word ptr [ebx + 2] ; 16bit displacement
                lea esi, [ebx + 4] ; end of displacement field(host)
                ; fall-through
    RM_Decode:
                ; displacement in edi
                movzx ecx, ah ; mod[2] reg[3] r/m[3]
                test ecx, 0100b
                jnz RM_Is1XX
                ; r/m = 0,b,i
                and ecx, 0010b ; Base = b ? BP : BX, R_BP = R_BX + 4
                movzx edx, word ptr R_BX[ecx * 2] ; actually word ptr is not needed
                movzx ecx, ah ; mod[2] reg[3] r/m[3]
                and ecx, 0001b ; Index = i ? DI : SI, R_DI = R_SI + 4
                movzx ecx, word ptr R_SI[ecx * 2]
                add edx, ecx
                jmp AddDisplacment
    RM_Is1XX:
                test ecx, 0010b
                jnz RM_Is11X
                ; r/m = 1,0,i
                and ecx, 0001b ; Index = i ? DI : SI, R_DI = R_SI + 4
                movzx edx, word ptr R_SI[ecx * 2]
                jmp AddDisplacment
    RM_Is11X:
                ; r/m = 1,1,~b
                and ecx, 0001b ; Base = b ? BP : BX, R_BP = R_BX + 4
                xor ecx, 1
                movzx edx, word ptr R_BX[ecx * 4]
                ; fall-through
    AddDisplacment:
                add edx, edi ; effective address(virtual) now in edx
                ; now edi free
                movzx ecx, SegmentType
                shl ecx, 4
                lea edx, MEMO[edx + ecx] ; effective address(host)
                jmp LeaveLabel
    MOD3:
                ; r/m = register
                lea esi, [ebx + 2] ; end of displacement(host) (No Displacement)
                movzx ecx, ah ; mod[2] reg[3] r/m[3], moved before jump to reuse code
                test al, 0001b ; first byte still in al, decide 16bit or 8bit register
                jnz RM_IsWordReg
                ; 8bit register
                and ecx, 0011b
                lea edx, REGB[ecx * 2] ; ACDB
                movzx ecx, ah
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add edx, ecx ; register host address now in edx
                jmp LeaveLabel
    RM_IsWordReg:
                ; 16bit register
                and ecx, 0111b
                lea edx, REGW[ecx * 2] ; register host address now in edx
                ; fall-through
    IF DisableFallThroughLeave
                jmp LeaveLabel
    ENDIF
ENDM

; ebx is flat addr in host machine
computeFlatIP MACRO
                movzx eax, R_CS
                shl eax, 4
                movzx ebx, R_IP
                lea ebx, MEMO[ebx + eax]
ENDM

; use ah
modifyFlagsInstruction MACRO instruction
                mov ah, R_FLAGS
                sahf
                instruction
                lahf
                mov R_FLAGS, ah
ENDM

; error case return address passed by ecx
; success will use ret
; flat ip in ebx
ArithLogic PROC
                movzx eax, word ptr [ebx]; read 2 bytes at once for later use, may exceed 1M, but we are in a emulator
                test eax, 11000100b ; only test low byte -- the first byte
                jz RegWithRegOrMem; must be 00xxx0xx, No Imm Op
                test eax, 01111100b
                jz ImmWithRegOrMem; must be 100000xx(with test 11000100b not zero), Imm to reg/mem
                test eax, 11000010b
                jz ImmToAcc; must be 00xxx10x(with test 11000100b not zero), Imm to accumulator Op
                jmp ecx; Other Instructions
    ImmToAcc:
                xor ecx, ecx
                test al, 0001b
                setnz cl
                lea edx, [R_AX] ; dest addr
                lea esi, [ebx + 1] ; src addr
                lea edi, [ecx + 2] ; delta ip
                ; now ebx free
                movzx ebx, al ; first byte contains op[3]
                jmp Operand
    ImmWithRegOrMem:
    RegWithRegOrMem:
                computeEffectiveAddress SrcIsRegOrImm, 0, R_DS
    SrcIsRegOrImm: ; not real src, d[1] decide real src
                mov edi, esi ; copy "end of displacement(host)" for delta ip
                sub edi, ebx ; compute delta ip, not yet count imm data, will be handled in SrcIsImm
                ; now ebx free
                test al, 10000000b ; first byte still in al
                jnz SrcIsImm
                ; Not Imm, Use Reg
                movzx ebx, al ; first byte contains op[3]
                shr ah, 2 ; not fully shift to eliminate index scaling
                movzx ecx, ah ; 00 mod[2] reg[3] x, moved before jump to reuse code
                test al, 0001b ; decide 16bit or 8bit register
                jnz REG_IsWordReg
                ; 8bit register
                and ecx, 0110b ; ecx = 00 mod[2] reg[3] x ; 0,2,4,6 -> ACDB 
                movzx ebx, ah
                and ebx, 1000b ; 0 -> L, 1 -> H
                shr ebx, 3
                lea esi, REGB[ecx + ebx] ; reg register host address now in esi
                jmp SRC_DEST ; first byte still in al
    REG_IsWordReg:
                ; 16bit register
                and ecx, 1110b
                lea esi, REGW[ecx] ; reg register host address now in esi
                jmp SRC_DEST ; first byte still in al
    SrcIsImm:
                movzx ebx, ah ; second byte contains op[3]
                ; compute delta ip
                xor ecx, ecx
                cmp al, 10000001b ; 100000 s[1] w[1], 00: +1, 01: +2, 10: +1, 11: +1
                sete cl
                lea edi, [edi + 1 + ecx] ; delta ip in edi
                ; imm data host address(end of displacement) already in esi
                jmp SRC_DEST ; first byte still in al
    SRC_DEST:
                ; first byte still in al
                test al, 10000000b;
                jnz Operand ; Imm to r/m, no need to exchange
                test al, 0010b; d[1] or s[1] (Imm to r/m case)
                jz Operand ; d = 0 no need to exchange
                xchg esi, edx ; put src in esi and dest in edx, for sub/sbb/cmp and write back
                ; fall-through
    Operand:           
                ; first byte still in al
                test al, 0001b ; decide 8bit or 16bit operand
                jnz OperandW ; word operand
                ; use 8bit partial reg for convenience
                ; generally that will be slower because of partial register stalls
                ; fortunately we don't need to read from cx or ecx, actually no stall occur
                mov cl, byte ptr [esi] ; src operand
                and ebx, 00111000b ; xx op[3] xxx, select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + ebx]
    OperandW:
                ; first byte still in al
                test al, 10000000b
                jz NotSignExt ; Not Imm to r/m
                test al, 0010b; s[1]
                jz NotSignExt
                movsx cx, byte ptr [esi] ; src operand, sign ext
                jmp OperandWExec
    NotSignExt:
                mov cx, word ptr [esi] ; src operand
                ; fall-through
    OperandWExec:
                and ebx, 00111000b ; xx op[3] xxx, select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + 4 + ebx]
    OpTable:
                ; could store diff to some near Anchor(e.g. OpTable) to save space
                ; but we use a straightforward method
                dword B_ADD, W_ADD
                dword B_OR, W_OR
                dword B_ADC, W_ADC
                dword B_SBB, W_SBB
                dword B_AND, W_AND
                dword B_SUB, W_SUB
                dword B_XOR, W_XOR
                dword B_CMP, W_CMP
    ByteOp:
        B_CMP:
                cmp byte ptr [edx], cl
                jmp WriteFlags
        B_XOR:
                xor byte ptr [edx], cl
                jmp WriteFlags
        B_SUB:
                sub byte ptr [edx], cl
                jmp WriteFlags
        B_AND:
                and byte ptr [edx], cl
                jmp WriteFlags
        B_SBB:
                mov ah, R_FLAGS
                sahf
                sbb byte ptr [edx], cl
                jmp WriteFlags
        B_ADC:
                mov ah, R_FLAGS
                sahf
                adc byte ptr [edx], cl
                jmp WriteFlags
        B_OR:
                or byte ptr [edx], cl
                jmp WriteFlags
        B_ADD:
                add byte ptr [edx], cl
                jmp WriteFlags
    WordOp:
        W_CMP:
                cmp word ptr [edx], cx
                jmp WriteFlags
        W_XOR:
                xor word ptr [edx], cx
                jmp WriteFlags
        W_SUB:
                sub word ptr [edx], cx
                jmp WriteFlags
        W_AND:
                and word ptr [edx], cx
                jmp WriteFlags
        W_SBB:
                mov ah, R_FLAGS
                sahf
                sbb word ptr [edx], cx
                jmp WriteFlags
        W_ADC:
                mov ah, R_FLAGS
                sahf
                adc word ptr [edx], cx
                jmp WriteFlags
        W_OR:
                or word ptr [edx], cx
                jmp WriteFlags
        W_ADD:
                add word ptr [edx], cx
                ; fall-through
    WriteFlags:
                lahf ; load flags into ah
                mov R_FLAGS, ah
                add R_IP, di
                ret
ArithLogic ENDP

Arith_INC_DEC PROC ; note: inc and dec is partial flags writer we need to load flags before inc or dec
                movzx eax, word ptr [ebx] ; read 2 byte at once, may exceed 1M, but we are in a emulator
                
                xor al, 01000000b 
                test al, 11110000b ; high 4 0100
                jz RegOnly
                xor al, 10111110b ; equiv to xor 11111110 at once
                test al, 11111110b 
                jz RegOrMem
                jmp ecx
    RegOnly:
                add R_IP, 1 ; 1byte long
                movzx ebx, al
                test al, 00001000b
                jnz RegOnlyDEC
                ; other bits in eax already clear
                ; INC
                ; note: load flags use ah
                modifyFlagsInstruction <inc word ptr REGW[ebx * 2]>
                ret
        RegOnlyDEC:
                modifyFlagsInstruction <dec word ptr REGW[ebx * 2 - 16]> ; 1 reg[3], *2 - 16
                ret
    RegOrMem:
                test ah, 00110000b
                jz Match
                jmp ecx ; other instructions
        Match:
                ; note: al lowest bit already w[1]
                computeEffectiveAddress INC_DEC_ComputeEA_Done, 0, R_DS
        INC_DEC_ComputeEA_Done:
                sub esi, ebx
                add R_IP, si
                test ah, 00001000b
                jnz RegOrMemDEC
                ; INC
                test al, 0001b
                jnz WordINC
                ; byte INC
                modifyFlagsInstruction <inc byte ptr [edx]>
                ret
            WordINC:
                modifyFlagsInstruction <inc word ptr [edx]>
                ret
        RegOrMemDEC:
                test al, 0001b
                jnz WordDEC
                ; byte DEC
                modifyFlagsInstruction <dec byte ptr [edx]>
                ret
        WordDEC:
                modifyFlagsInstruction <dec word ptr [edx]>
                ret
Arith_INC_DEC ENDP

; uses ah
GenerateJmpConditional MACRO jmp_cc
                movsx di, ah
                mov ah, R_FLAGS
                sahf
                jmp_cc Jmp_Short_Rel8_Inner
                add R_IP, 2 ; instruction length = 2 bytes
                ret
ENDM

; flat ip in ebx
; NOTE: cs can be changed!
; error case return address passed by ecx
; success will use ret
ControlTransfer PROC
                movzx eax, word ptr [ebx] ; read 2 byte at once, may exceed 1M, but we are in a emulator
                cmp al, 0E8h   ; parse instruction type
                je Call_Direct_Near
                cmp al, 09Ah
                je Call_Direct_Far
                cmp al, 0FFh
                je Call_Jmp_Indirect
                cmp al, 0EBh
                je Jmp_Short_Rel8
                cmp al, 0E9h
                je Jmp_Near_Rel16
                cmp al, 0EAh
                je Jmp_Direct_Far 
    FOR x, <70h,71h,72h,73h,74h,75h,76h,77h,78h,79h,7ah,7bh,7ch,7dh,7eh,7fh> ; conditional jmp
                cmp al, x
                je Jmp&x
    ENDM
                movzx edi, word ptr [ebx + 1] ; pop imm16 bytes
                cmp al, 0C2h
                je Ret_Near
                cmp al, 0CAh
                ; edi already load
                je Ret_Far

                xor edi, edi ; pop 0 byte
                cmp al, 0C3h
                je Ret_Near
                cmp al, 0CBh
                ; edi already clear
                je Ret_Far

                jmp ecx ; other instructions
                
    Jmp70h:     GenerateJmpConditional jo
    Jmp71h:     GenerateJmpConditional jno
    Jmp72h:     GenerateJmpConditional jb
    Jmp73h:     GenerateJmpConditional jae
    Jmp74h:     GenerateJmpConditional je
    Jmp75h:     GenerateJmpConditional jne
    Jmp76h:     GenerateJmpConditional jbe
    Jmp77h:     GenerateJmpConditional ja
    Jmp78h:     GenerateJmpConditional js
    Jmp79h:     GenerateJmpConditional jns
    Jmp7ah:     GenerateJmpConditional jpe
    Jmp7bh:     GenerateJmpConditional jpo
    Jmp7ch:     GenerateJmpConditional jl
    Jmp7dh:     GenerateJmpConditional jge
    Jmp7eh:     GenerateJmpConditional jle
    Jmp7fh:     GenerateJmpConditional jg

    Jmp_Short_Rel8:
                movsx di, ah
    Jmp_Short_Rel8_Inner:
                add di, 2 ; instruction length = 2 bytes
                add R_IP, di ; ip += rel8 sign extended to 16bit (relative to next instruction)
                ret
    Call_Direct_Near: ; near direct is ip relative, cannot reuse indirect code
                movzx edx, R_SP
                movzx ecx, R_SS
                sub R_SP, 2 ; write after read to avoid stall
                shl ecx, 4

                mov si, R_IP
                add si, 3 ; instruction length = 3 bytes
                mov word ptr MEMO[edx + ecx - 2], si
                add si, word ptr [ebx + 1]
                mov R_IP, si ; write back
                ret ; not reuse code
    Jmp_Near_Rel16:
                ; ip += displacement
                mov si, R_IP
                add si, 3 ; instruction length = 3 bytes
                add si, word ptr [ebx + 1]
                mov R_IP, si ; write back
                ret
    Call_Direct_Far:
                lea edx, [ebx + 1]
                lea esi, [ebx + 5]
                jmp Call_Indirect_Far ; reuse code
    Jmp_Direct_Far:
                lea edx, [ebx + 1]
                lea esi, [ebx + 5]
                jmp Jmp_Indirect_Far ; reuse code
    Call_Jmp_Indirect:
                test ah, 00110000b
                jz NotMatch ; xx00xxxx
                xor ah, 00110000b
                test ah, 00110000b
                jz NotMatch ; xx11xxxx
                computeEffectiveAddress Control_Flow_EA_Done, 0, R_DS
                ; fall-through
        Control_Flow_EA_Done:
                ; IMPORTANT: ah xor with 00110000b
                test ah, 00100000b
                jz Jmp_Indirect ; xx10sxxx
                ; xx01sxxx
                test ah, 00001000b
                jz Call_Indirect_Near ; xx010xxx
                jmp Call_Indirect_Far ; xx011xxx
            Jmp_Indirect:
                ; xx10sxxx
                test ah, 00001000b
                jz Jmp_Indirect_Near ; xx100xxx
                jmp Jmp_Indirect_Far ; xx101xxx
        NotMatch:
                jmp ecx
    Call_Indirect_Near:
                sub esi, ebx ; esi-ebx is command length
                add si, R_IP ; add to R_IP to get offset of next instruction
                ; now ebx free
                ; push ip
                movzx ebx, R_SP
                movzx ecx, R_SS
                sub R_SP, 2 ; write after read to avoid stall
                shl ecx, 4
                mov word ptr MEMO[ebx + ecx - 2], si
                ; fall-through
    Jmp_Indirect_Near:
                mov ax, word ptr [edx] ; load offset into cx
                mov R_IP, ax ; write back new ip
                ret

    Call_Indirect_Far:
                sub esi, ebx ; esi-ebx is command length
                add si, R_IP ; add to R_IP to get offset of next instruction
                ; now ebx free
                ; push cs, then push ip
                movzx ebx, R_SP
                movzx ecx, R_SS
                sub R_SP, 4 ; write after read to avoid stall
                shl ecx, 4

                movzx eax, R_CS
                shl eax, 16
                or eax, esi ; avoid partial register write then read whole register
                mov dword ptr MEMO[ebx + ecx - 4], eax
                ; fall-through
    Jmp_Indirect_Far:
                mov eax, dword ptr [edx]
                mov R_IP, ax
                shr eax, 16
                mov R_CS, ax
                ret

    Ret_Near:       
                movzx edx, R_SP
                movzx ecx, R_SS
                shl ecx, 4
                mov ax, word ptr MEMO[edx + ecx] ; rtn addr in ax

                mov R_IP, ax
                add di, 2 ; pop n + 2 byte
                add R_SP, di
                ret
    Ret_Far:
                movzx edx, R_SP
                movzx ecx, R_SS
                shl ecx, 4
                mov eax, dword ptr MEMO[edx + ecx]; rtn addr (high[16] = cs, low[16] = ip ) in eax

                mov R_IP, ax
                shr eax, 16
                mov R_CS, ax

                mov R_IP, ax
                add di, 4 ; pop n + 4 byte
                add R_SP, di
                ret
ControlTransfer ENDP

; error case return address passed by ecx
; flat ip in ebx
; success will use ret
DataTransferMOV PROC
                movzx eax, word ptr [ebx] ; read 2 byte at once, may exceed 1M, but we are in a emulator
                xor al, 10001000b
                test al, 11111100b ; high 6 100010
                jz RegWithRegOrMem ; 100010xx
                ; not xor
                test al, 11111001b
                jz SegRegWithRegOrMem; 100011x0, previous test with 11111100b not zero, thus test with 0100b must not zero
                xor al, 00101000b ; equiv to xor 10100000b at once
                test al, 11111100b ; high 6 101000
                jz MemWithAccumulator
                xor al, 00010000b ; equiv to xor 10110000b at once
                test al, 11110000b ; high 4 1011
                jz ImmToReg
                xor al, 01110110b ; equiv to xor 11000110b at once
                test al, 11111110b ; high 7 1100011
                jz ImmToRegOrMem
                jmp ecx         
    ImmToRegOrMem:
                or al, 10000000b ; set flag to reuse code, repeat macro maybe a little faster
                jmp WithRegOrMem
    SegRegWithRegOrMem:
                or al, 0001b ; SegReg case don't have w[1], manually set lowest bit
                ; fall-through
    RegWithRegOrMem:
                ; fall-through
    WithRegOrMem:
                computeEffectiveAddress SrcIsRegOrImm, 0, R_DS
    SrcIsRegOrImm: ; not real src, d[1] decide real src
                test al, 10000000b ; test flag
                jnz SrcIsImm
                ; Not Imm, Use Reg
                ; compute delta ip
                sub esi, ebx
                add R_IP, si
                ; now ebx, esi free

                shr ah, 2 ; not fully shift to eliminate index scaling
                movzx ecx, ah ; 00 mod[2] reg[3] x, moved before jump to reuse code

                test al, 0100b ; check if segment register
                jnz SegReg

                test al, 0001b ; decide 16bit or 8bit register
                jnz REG_IsWordReg
                ; 8bit register
                and ecx, 0110b ; ecx = 00 mod[2] reg[3] x ; 0,2,4,6 -> ACDB
                movzx ebx, ah
                and ebx, 1000b ; 0 -> L, 1 -> H
                shr ebx, 3

                test al, 0010b ; d[1]
                jnz ToByteReg
                ; from byteReg
                mov al, byte ptr REGB[ecx + ebx]
                mov byte ptr [edx], al
                ret
    ToByteReg:
                mov al, byte ptr [edx]
                mov byte ptr REGB[ecx + ebx], al
                ret
    REG_IsWordReg:
                ; 16bit register
                and ecx, 1110b
                test al, 0010b ; d[1]
                jnz ToWordReg
                ; from WordReg
                mov ax, word ptr REGW[ecx]
                mov word ptr [edx], ax
                ret
    ToWordReg:
                mov ax, word ptr [edx]
                mov word ptr REGW[ecx], ax
                ret
    SrcIsImm:
                mov cx, word ptr [esi] ; read 2 byte at once to reuse code, may exceed 1M, but we are in a emulator
                test al, 0001b ; w[1]
                jnz WordSrcImm
                ; Byte Imm
                add esi, 1
                sub esi, ebx
                add R_IP, si
                mov byte ptr [edx], cl
                ret
    WordSrcImm:
                add esi, 2
                sub esi, ebx
                add R_IP, si
                mov word ptr [edx], cx
                ret
    SegReg:
                and ecx, 0110b ; reg[3] = 0 seg[2]
                test al, 0010b ; d[1]
                jnz ToSegReg
                mov ax, word ptr REGS[ecx]
                mov word ptr [edx], ax
                ret
    ToSegReg:
                mov ax, word ptr [edx]
                mov word ptr REGS[ecx], ax
                ret
    ImmToReg:
                test al, 1000b ; w[1]
                jnz WordImmToReg
                ; byte Imm
                ; ebx + 1 already in ah, ebx free
                add R_IP, 2
                movzx ecx, al ; 0000 w[1] reg[3]
                and ecx, 0011b
                movzx ebx, al
                and ebx, 0100b
                shr ebx, 2
                mov byte ptr REGB[ecx * 2 + ebx], ah
                ret
    WordImmToReg:
                add R_IP, 3
                movzx ecx, al
                and ecx, 0111b
                mov ax, word ptr [ebx + 1]
                mov word ptr REGW[ecx * 2], ax
                ret
    MemWithAccumulator:
                add R_IP, 3
                movzx edx, word ptr [ebx + 1]
                movzx ecx, R_DS
                shl ecx, 4
                test al, 0010b ; 0 to accumulator
                jnz FromAccumulator
                mov bx, word ptr MEMO[edx + ecx] ; read 2 byte at once to reuse code, may exceed 1M, but we are in a emulator
                test al, 0001b ; w[1]
                jnz ToAX
                ; to AL
                mov R_AL, bl
                ret
    ToAX:
                mov R_AX, bx
                ret
    FromAccumulator:
                test al, 0001b ; w[1]
                jnz FromAX
                ; from AL
                mov bl, R_AL
                mov byte ptr MEMO[edx + ecx], bl
                ret
    FromAX:
                mov bx, R_AX
                mov word ptr MEMO[edx + ecx], bx
                ret

DataTransferMOV ENDP

; error case return address passed by ecx
; flat ip in ebx
; success will use ret
DataTransferStack PROC
                movzx eax, word ptr [ebx] ; read 2 byte at once, may exceed 1M, but we are in a emulator
                xor al, 01010000b
                test al, 11110000b ; high 4 0101
                jz Register ; 0101xxxx
                xor al, 01010110b ; equiv to xor 00000110b at once
                test al, 11100110b ; 000xx11x
                jz SegmentRegister
                xor al, 10001001b ; equiv to xor 10001111b at once
                jz PopRegOrMem
                xor al, 01110000b ; equiv to xor 11111111b at once
                jz PushRegOrMem
                jmp ecx
    Register:
                add R_IP, 1
                movzx esi, R_SP
                movzx ebx, R_SS
                shl ebx, 4

                movzx ecx, al
                and ecx, 0111b
                test al, 1000b
                jnz PopRegister
                ; push
                mov ax, word ptr REGW[ecx * 2]
                sub R_SP, 2 ; push word
                mov word ptr MEMO[ebx + esi - 2], ax
                ret
    PopRegister:
                ; pop
                mov ax, word ptr MEMO[ebx + esi]
                add R_SP, 2 ; pop word
                mov word ptr REGW[ecx * 2], ax
                ret
    SegmentRegister:
                add R_IP, 1
                movzx esi, R_SP
                movzx ebx, R_SS
                shl ebx, 4

                movzx ecx, al
                and ecx, 00011000b
                shr ecx, 2 ; not fully shift
                test al, 0001b
                jnz PopSegmentRegister
                ; push
                mov ax, word ptr REGS[ecx]
                sub R_SP, 2
                mov word ptr MEMO[ebx + esi - 2], ax
                ret
    PopSegmentRegister:
                ; pop
                mov ax, word ptr MEMO[ebx + esi]
                add R_SP, 2
                mov word ptr REGS[ecx], ax
                ret
    PopRegOrMem:
                test ah, 00111000b
                jnz NotMatch

                or al, 10000000b ; set flag for code reuse
                jmp EA_Compute
        PopRegOrMemEADone:
                mov ax, word ptr MEMO[ebx + esi]
                add R_SP, 2
                mov word ptr [edx], ax
                ret
    PushRegOrMem:
                xor ah, 00110000b
                test ah, 00111000b
                jnz NotMatch
                
                jmp EA_Compute
        PushRegOrMemEADone:
                mov ax, word ptr [edx]
                sub R_SP, 2
                mov word ptr MEMO[ebx + esi - 2], ax
                ret
    EA_Compute:
                or al, 0001b ; set lowest bit indicate word register in r/m, which is xor cleared on instruction type check
                computeEffectiveAddress EA_Done, 0, R_DS
        EA_Done:
                sub esi, ebx
                add R_IP, si
                movzx esi, R_SP
                movzx ebx, R_SS
                shl ebx, 4

                test al, 10000000b
                jz PushRegOrMemEADone
                jmp PopRegOrMemEADone
    NotMatch:
                jmp ecx
DataTransferStack ENDP

FlagInstruction PROC
                mov al, byte ptr [ebx]
                cmp al, 0F8h
                je ProcessClc
                cmp al, 0F9h
                je ProcessStc
                jmp ecx
    ProcessClc:
                add R_IP, 1
                modifyFlagsInstruction <clc>
                
                ret
    ProcessStc:
                add R_IP, 1
                modifyFlagsInstruction <stc>
                ret
FlagInstruction ENDP

XchgInstruction PROC
                movzx eax, word ptr [ebx] ; read 2 byte at once, may exceed 1M, but we are in a emulator
                xor al, 10000110b
                test al, 11111110b
                jz XchgRegOrMem
                xor al, 00010110b ; equiv to xor 10010000b
                test al, 11111000b
                jz XchgAX
                jmp ecx
XchgAX:         
                ; other bits in eax already clear
                add R_IP, 1
                mov bx, R_AX
                xchg bx, word ptr REGW[eax * 2]
                mov R_AX, bx
                ret
XchgRegOrMem:
                computeEffectiveAddress XchgRegOrMemEADone, 0, R_DS
XchgRegOrMemEADone:
                sub esi, ebx
                add R_IP, si
                ; now ebx, esi free

                shr ah, 2 ; not fully shift to eliminate index scaling
                movzx ecx, ah ; 00 mod[2] reg[3] x, moved before jump to reuse code

                test al, 0001b ; decide 16bit or 8bit register
                jnz XchgRegOrMemWord
                ; 8bit register
                and ecx, 0110b ; ecx = 00 mod[2] reg[3] x ; 0,2,4,6 -> ACDB
                movzx ebx, ah
                and ebx, 1000b ; 0 -> L, 1 -> H
                shr ebx, 3

                mov al, byte ptr [edx]
                xchg al, byte ptr REGB[ecx + ebx]
                mov byte ptr [edx], al
                ret
XchgRegOrMemWord:
                and ecx, 1110b
                mov ax, word ptr [edx]
                xchg ax, word ptr REGW[ecx]
                mov word ptr [edx], ax

XchgInstruction ENDP

computeEffectiveAddressUnitTest MACRO
                LOCAL callback, L1, L2, L3, L4, L5, L6
                mov ebx, offset MEMO
                mov byte ptr [ebx + 2], 0
                mov byte ptr [ebx + 3], 1
                mov ah, 00000110b
                push offset L1
                computeEffectiveAddress callback, 1, R_DS
    L1:
                mov ebx, offset MEMO
                mov byte ptr [ebx + 2], 10
                mov byte ptr [ebx + 3], 0
                mov ah, 00000110b
                push offset L2
                computeEffectiveAddress callback, 1, R_DS
    L2:
                mov ebx, offset MEMO
                mov ah, 00000000b
                mov R_BX, 4
                mov R_SI, 3
                push offset L3
                computeEffectiveAddress callback, 1, R_DS
    L3:
                mov ebx, offset MEMO
                mov ah, 10000000b
                mov byte ptr [ebx + 2], 10
                mov byte ptr [ebx + 3], 0
                mov R_BX, 4
                mov R_SI, 3
                push offset L4
                computeEffectiveAddress callback, 1, R_DS
    L4:
                mov ebx, offset MEMO
                mov ah, 00000101b
                mov R_DI, 5
                push offset L5
                computeEffectiveAddress callback, 1, R_DS
    L5:
                mov ebx, offset MEMO
                mov ah, 00000111b
                mov R_BP, 9
                push offset L6
                computeEffectiveAddress callback, 1, R_DS
    L6:
                ret
    callback:
                INVOKE printf, offset debugMsg, offset MEMO, edx, esi, 0
                ret
ENDM

include term.asm
include binloader.asm

main PROC
                INVOKE LoadBinaryIntoEmulator, ADDR MEMO, ADDR floppyPath
                INVOKE InitEmuScreen ; initialize terminal
                lea edi, [MEMO + 0b8000h]
                mov ecx, 80*25
                mov ax, 0
                rep lodsw          ; clrscr
ExecLoop:       ; first, draw video memory
                INVOKE WriteEmuScreen, ADDR [MEMO + 0b8000h]

                ; execute next instruction
                pushad

                computeFlatIP
                movzx eax, byte ptr [ebx]
                cmp eax, 0F4h
                je EmulatorHalt

                push offset Executed

                mov ecx, OFFSET ExecIncDec
                jmp ArithLogic
ExecIncDec:
                mov ecx, OFFSET ExecControl
                jmp Arith_INC_DEC
ExecControl:    
                mov ecx, OFFSET ExecData
                jmp ControlTransfer
ExecData:       
                mov ecx, OFFSET ExecFlag
                jmp DataTransferMOV
ExecFlag:       
                mov ecx, OFFSET ExecPushPop
                jmp FlagInstruction
ExecPushPop:    
                mov ecx, OFFSET ExecXchg
                jmp DataTransferStack
ExecXchg:       
                mov ecx, OFFSET ExecUD
                jmp XchgInstruction
ExecUD:         
                mov ecx, MB_ICONERROR
                or ecx, MB_OK
                INVOKE MessageBox, NULL, ADDR UDMsg, ADDR UDMsgTitle, ecx
                add R_IP, 1 ; skip this opcode
                add esp, 4; pop offset Executed
Executed:
                popad
                jmp ExecLoop

EmulatorHalt:
                INVOKE MessageBox, NULL, ADDR haltMsg, ADDR haltMsgTitle, MB_OK
                INVOKE ExitProcess, 0
                ret
main ENDP
END main
