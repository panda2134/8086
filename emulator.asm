.686
.model flat, stdcall
option casemap:none

includelib      msvcrt.lib
printf          PROTO C :ptr byte, :VARARG

.data
debugMsg        byte "%d %d %d %d", 0AH, 0DH, 0
invalidOpMsg    byte "Invalid Operation!", 0Dh, 0Ah, 0
REGB            label byte
REGW            label word
R_AX            word 0
R_CX            word 0
R_DX            word 0
R_BX            word 0
R_SP            word 0
R_BP            word 0
R_SI            word 0
R_DI            word 0

R_ES            word 0
R_CS            word 0FFFFH
R_SS            word 0
R_DS            word 0

R_FLAGS         byte 0

R_IP            word 0

MEMO            byte 1048576 DUP(?)
MEMO_Guard      byte 00FFH

.code
; mod[2] xxx r/m[3] passed by ah, start of instruction passed by ebx
; effective address returned by edx, end of displacement returned by esi
computeEffectiveAddress MACRO LeaveLabel, DisableFallThroghLeave
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
                lea esi, [ebx + 4] ; End of displacement
                xor edx, edx ; clear edx for displacement only
                jmp AddDisplacment
    NoDisplacement:
                xor edi, edi ; common case, no displacement
                lea esi, [ebx + 2] ; End of displacement
                jmp RM_Decode
    MOD123:
                cmp cl, 1
                jne MOD23
                ; mod = 01
                movzx edi, byte ptr [ebx + 2] ; 8bit displacement
                lea esi, [ebx + 3] ; End of displacement
                jmp RM_Decode
    MOD23:
                cmp cl, 2
                jne MOD3
                ; mod = 10
                movzx edi, word ptr [ebx + 2] ; 16bit displacement
                lea esi, [ebx + 4] ; End of displacement
                ; fall-through
    RM_Decode:
                ; displacement in edi
                movzx ecx, ah ; mod[2] reg[3] r/m[3]
                test ecx, 0100b
                jnz RM_Is1XX
                ; r/m = 0xx
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
                ; r/m = 10x
                and ecx, 0001b ; Index = i ? DI : SI, R_DI = R_SI + 4
                movzx edx, word ptr R_SI[ecx * 2]
                jmp AddDisplacment
    RM_Is11X:
                ; r/m = 11x
                and ecx, 0001b ; Base = b ? BP : BX, R_BP = R_BX + 4
                movzx edx, word ptr R_BX[ecx * 4]
                ; fall-through
    AddDisplacment:
                add edx, edi ; (virtual) effective address now in edx
                ; now edi free
                add edx, offset MEMO
                movzx ecx, R_DS ; (virtual) data segment, may be override, TODO
                add edx, ecx
                jmp LeaveLabel
    MOD3:
                ; r/m = register
                lea esi, [ebx + 2] ; End of displacement (No Displacement)
                ; now ebx free
                movzx ecx, ah ; mod[2] reg[3] r/m[3], moved before jump to reuse code
                test al, 0001b ; first byte still in al, decide 16bit or 8bit register
                jnz RM_IsWordReg
                ; 8bit register
                and ecx, 0011b
                lea edx, REGB[ecx * 2] ; ACDB
                movzx ecx, ah
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add edx, ecx ; register "address" now in edx
                jmp LeaveLabel
    RM_IsWordReg:
                ; 16bit register
                and ecx, 0111b
                lea edx, REGW[ecx * 2] ; register "address" now in edx
                ; fall-through
    IF DisableFallThroghLeave
                    jmp LeaveLabel
    ENDIF
ENDM

; ebx is flat addr in host machine
computeFlatIP MACRO
                movzx eax, R_CS
                movzx ebx, R_IP
                lea ebx, MEMO[ebx + eax * 16]
ENDM

; error case return address passed by ecx
; success will use ret
ArithLogic PROC
                computeFlatIP
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
                lea edi, [ebx + 2 + ecx] ; new pc
                ; now ebx free
                movzx ebx, al ; first byte contains op[3]
                jmp Operand
    ImmWithRegOrMem:
    RegWithRegOrMem:
                computeEffectiveAddress SrcIsRegOrImm, 0
    SrcIsRegOrImm:
                mov edi, esi; save "End of displacement" for new ip
                test al, 10000000b ; first byte still in al
                jnz SrcIsImm
                ; Not Imm, Use Reg
                movzx ebx, al ; first byte contains op[3]
                shr ah, 3
                movzx ecx, ah ; 000 mod[2] reg[3], moved before jump to reuse code
                test al, 0001b ; decide 16bit or 8bit register
                jnz REG_IsWordReg
                ; 8bit register
                and ecx, 0011b ; ecx = 000 mod[2] reg[3]
                lea esi, REGB[ecx * 2] ; ACDB
                movzx ecx, ah
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add esi, ecx ; reg register "address" now in esi
                jmp SRC_DEST ; first byte still in al
    REG_IsWordReg:
                ; 16bit register
                and ecx, 0111b
                lea esi, REGW[ecx * 2] ; reg register "address" now in esi
                jmp SRC_DEST ; first byte still in al
    SrcIsImm:
                movzx ebx, ah ; second byte contains op[3]
                ; compute new ip
                xor ecx, ecx
                cmp al, 10000011b ; 100000 s[1] w[1], 00: +1, 01: +2, 10: +1, 11: +1
                sete cl
                lea edi, [edi + 1 + ecx] ; new ip in edi
                ; (virtual) imm data address already in esi
                add esi, offset MEMO
                movzx ecx, word ptr R_CS
                add esi, ecx
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
                ; use 8bit partia reg for convenience
                ; generally that will be slower because of partial register stalls
                ; fortunately we don't need to read from cx or ecx, actually no stall occur
                mov cl, byte ptr [edx] ; dest operand
                mov ch, byte ptr [esi] ; src operand
                and ebx, 00111000b ; xx op[3] xxx, select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + ebx]
    OperandW:
                ; first byte still in al
                movzx ecx, word ptr [edx]; only use cx
                test al, 10000000b
                jz NotSignExt ; Not Imm to r/m
                test al, 0010b; s[1]
                jz NotSignExt
                movsx si, byte ptr [esi] ; src operand, no need to preserve its addr
                jmp OperandWExec
    NotSignExt:
                mov si, word ptr [esi]
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
                cmp cl, ch
                jmp WriteFlags
        B_XOR:
                xor cl, ch
                jmp WriteBackB
        B_SUB:
                sub cl, ch
                jmp WriteBackB
        B_AND:
                and cl, ch
                jmp WriteBackB
        B_SBB:
                sbb cl, ch
                jmp WriteBackB
        B_ADC:
                adc cl, ch
                jmp WriteBackB      
        B_OR:
                or cl, ch
                jmp WriteBackB
        B_ADD:
                add cl, ch
                ; fall-through
    WriteBackB:
                mov byte ptr [edx], cl
                jmp WriteFlags
    WordOp:
        W_CMP:
                cmp cx, si
                jmp WriteFlags
        W_XOR:
                xor cx, si
                jmp WriteBackW
        W_SUB:
                sub cx, si
                jmp WriteBackW
        W_AND:
                and cx, si
                jmp WriteBackW
        W_SBB:
                sbb cx, si
                jmp WriteBackW
        W_ADC:
                adc cx, si
                jmp WriteBackW
        W_OR:
                or cx, si
                jmp WriteBackW
        W_ADD:
                add cx, si
                ; fall-through
    WriteBackW:
                mov word ptr [edx], cx
                ; fall-through
    WriteFlags:
                lahf ; load flags into ah
                mov R_FLAGS, ah
                computeFlatIP
                sub edi, ebx
                add R_IP, di
                ret
ArithLogic ENDP


; NOTE: cs can be changed!
ControlTransfer PROC
                computeFlatIP
                movzx eax, byte ptr [ebx] ; read 1 byte into ax, and check type of instruction
                cmp ax, 11101000b   ; parse instruction type
                je Call_Direct_Near
                cmp ax, 10011010b
                je Call_Direct_Far
                cmp ax, 0FFh
                je Call_Indirect
Call_Direct_Near:
                ; first, push rtn addr (16bit) into stack
                movzx edx, R_SP
                sub edx, 2
                ; todo: exception when edx < 2
                mov ecx, ebx
                add ecx, 3 ; instruction length = 3 bytes
                mov word ptr [MEMO + edx], cx
                mov R_IP, cx ; ip is next instruction
                ; write back new SP
                mov R_SP, dx
                ; then retrieve displacement
                mov cx, [ebx + 1]
                ; ip += displacement
                add R_IP, cx
                jmp ControlTransfer_Done
Call_Direct_Far:
                ; push cs, then push rtn addr
                movzx edx, R_SP
                sub edx, 4
                ; todo: exception when edx < 4
                mov cx, R_IP
                add cx, 5
                mov word ptr [MEMO + edx], cx
                mov cx, R_CS
                mov word ptr [MEMO + 2 +edx], cx
                ; write back new SP
                mov R_SP, dx
                ; retrieve new disp and cs
                mov cx, [ebx + 1]
                mov dx, [ebx + 3]
                ; ip := displacement; change cs
                mov R_IP, cx
                mov R_CS, dx
                jmp ControlTransfer_Done
Call_Indirect:
                movzx ecx, byte ptr [ebx + 1]
                shr ecx, 3
                and ecx, 111b
                cmp ecx, 010b ; check for xx010xxx
                je Call_Indirect_Near
                cmp ecx, 011b ; check for xx011xxx
                je Call_Indirect_Far
                ret ; other instructions
Call_Indirect_Near:

Call_Indirect_Far:

ControlTransfer_Done:
                ret
ControlTransfer ENDP

computeEffectiveAddressUnitTest MACRO
                LOCAL callback, L1, L2, L3, L4, L5, L6
                mov ebx, offset MEMO
                mov byte ptr [ebx + 2], 0
                mov byte ptr [ebx + 3], 1
                mov ah, 00000110b
                push offset L1
                computeEffectiveAddress callback, 1
    L1:
                mov ebx, offset MEMO
                mov byte ptr [ebx + 2], 10
                mov byte ptr [ebx + 3], 0
                mov ah, 00000110b
                push offset L2
                computeEffectiveAddress callback, 1
    L2:
                mov ebx, offset MEMO
                mov ah, 00000000b
                mov R_BX, 4
                mov R_SI, 3
                push offset L3
                computeEffectiveAddress callback, 1
    L3:
                mov ebx, offset MEMO
                mov ah, 10000000b
                mov byte ptr [ebx + 2], 10
                mov byte ptr [ebx + 3], 0
                mov R_BX, 4
                mov R_SI, 3
                push offset L4
                computeEffectiveAddress callback, 1
    L4:
                mov ebx, offset MEMO
                mov ah, 00000101b
                mov R_DI, 5
                push offset L5
                computeEffectiveAddress callback, 1
    L5:
                mov ebx, offset MEMO
                mov ah, 00000111b
                mov R_BP, 9
                push offset L6
                computeEffectiveAddress callback, 1
    L6:
                ret
    callback:
                INVOKE printf, offset debugMsg, offset MEMO, edx, esi, 0
                ret
ENDM
run:
                computeEffectiveAddressUnitTest
end				run