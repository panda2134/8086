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
R_CS            word 0FFFFH
R_SS            word 0
R_DS            word 0

R_FLAGS         byte 0

R_IP            word 0

MEMO            byte 1048576 DUP(?)
MEMO_Guard      byte 00FFH

.code

pushEmulatorStack MACRO val
                IFIDNI <val>, <ax>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                IFIDNI <val>, <bx>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                
                IFIDNI <val>, <ah>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                IFIDNI <val>, <bh>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                
                IFIDNI <val>, <al>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                IFIDNI <val>, <bl>
                    ECHO Error: cannot use the given reg here
                    EXITM
                ENDIF
                push eax
                push ebx
                movzx eax, [R_SS]
                movzx ebx, [R_SP]
                sub ebx, TYPE val
                shl eax, 4
                add eax, ebx
                and eax, 0FFFFFH ; eax is new stack top in emulator
                mov R_SP, bx    ; write back new SP
                mov MEMO[eax], val ; put onto stack
                pop ebx
                pop eax
pushEmulatorStack ENDM

; mod[2] xxx r/m[3] passed by ah, start of instruction(host) passed by ebx
; not modified ah and ebx
; effective address(host) returned by edx, end of displacement field(host) returned by esi
computeEffectiveAddress MACRO LeaveLabel, DisableFallThroughLeave, SegmentType
                IFB <SegmentType>
                    SegmentType EQU R_DS
                ENDIF
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
                lea edi, [ecx + 2] ; delta ip
                ; now ebx free
                movzx ebx, al ; first byte contains op[3]
                jmp Operand
    ImmWithRegOrMem:
    RegWithRegOrMem:
                computeEffectiveAddress SrcIsRegOrImm, 0
    SrcIsRegOrImm: ; not real src, d[1] decide real src
                mov edi, esi ; copy "end of displacement(host)" for delta ip
                sub edi, ebx ; compute delta ip, not yet count imm data
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
                cmp al, 10000011b ; 100000 s[1] w[1], 00: +1, 01: +2, 10: +1, 11: +1
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
                mov al, byte ptr [esi] ; src operand
                and ebx, 00111000b ; xx op[3] xxx, select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + ebx]
    OperandW:
                ; first byte still in al
                test al, 10000000b
                jz NotSignExt ; Not Imm to r/m
                test al, 0010b; s[1]
                jz NotSignExt
                movsx ax, byte ptr [esi] ; src operand, sign ext
                jmp OperandWExec
    NotSignExt:
                mov ax, word ptr [esi] ; src operand
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
                cmp byte ptr [edx], al
                jmp WriteFlags
        B_XOR:
                xor byte ptr [edx], al
                jmp WriteFlags
        B_SUB:
                sub byte ptr [edx], al
                jmp WriteFlags
        B_AND:
                and byte ptr [edx], al
                jmp WriteFlags
        B_SBB:
                sbb byte ptr [edx], al
                jmp WriteFlags
        B_ADC:
                adc byte ptr [edx], al
                jmp WriteFlags
        B_OR:
                or byte ptr [edx], al
                jmp WriteFlags
        B_ADD:
                add byte ptr [edx], al
                jmp WriteFlags
    WordOp:
        W_CMP:
                cmp word ptr [edx], ax
                jmp WriteFlags
        W_XOR:
                xor word ptr [edx], ax
                jmp WriteFlags
        W_SUB:
                sub word ptr [edx], ax
                jmp WriteFlags
        W_AND:
                and word ptr [edx], ax
                jmp WriteFlags
        W_SBB:
                sbb word ptr [edx], ax
                jmp WriteFlags
        W_ADC:
                adc word ptr [edx], ax
                jmp WriteFlags
        W_OR:
                or word ptr [edx], ax
                jmp WriteFlags
        W_ADD:
                add word ptr [edx], ax
                ; fall-through
    WriteFlags:
                lahf ; load flags into ah
                mov R_FLAGS, ah
                add R_IP, di
                ret
ArithLogic ENDP


; NOTE: cs can be changed!
ControlTransfer PROC
                computeFlatIP
                movzx eax, byte ptr [ebx] ; read 1 byte into ax, and check type of instruction

                cmp ax, 0E8h   ; parse instruction type
                je Call_Direct_Near
                cmp ax, 09Ah
                je Call_Direct_Far
                cmp ax, 0FFh
                je Call_Jmp_Indirect
                cmp ax, 0EBh
                je Jmp_Short_Rel8
                cmp ax, 0E9h
                je Jmp_Near_Rel16
                cmp ax, 0EAh
                je Jmp_Direct_Far
                ret ; other instructions
Jmp_Short_Rel8:
                movsx di, byte ptr [ebx + 1]
                add R_IP, di ; ip += rel8 sign extended to 16bit
                add R_IP, 2  ; next instruction -> +2
                jmp ControlTransfer_Done
Call_Direct_Near:
                ; first, push rtn addr (16bit) into stack
                ; todo: exception when edx < 2
                mov cx, R_IP
                add cx, 3 ; instruction length = 3 bytes
                pushEmulatorStack cx
Jmp_Near_Rel16:
                mov R_IP, cx ; now ip points to the next instruction
                ; then retrieve displacement
                mov di, word ptr [ebx + 1]
                ; ip += displacement
                add R_IP, di
                jmp ControlTransfer_Done
Call_Direct_Far:
                ; push cs, then push rtn addr
                mov cx, R_CS
                pushEmulatorStack cx
                mov cx, R_IP
                add cx, 5
                pushEmulatorStack cx
Jmp_Direct_Far:
                ; retrieve new disp and cs
                mov ecx, dword ptr [ebx + 1]
                ; ip := displacement; change cs
                mov R_IP, cx
                shr ecx, 16
                mov R_CS, cx
                jmp ControlTransfer_Done
Call_Jmp_Indirect:
                mov ah, byte ptr [ebx + 1]
                computeEffectiveAddress Control_Flow_EA_Done, 1
Control_Flow_EA_Done:
                movzx ecx, ah
                shr ecx, 3
                and ecx, 111b
                cmp ecx, 010b ; check for xx010xxx
                je Call_Indirect_Near
                cmp ecx, 011b ; check for xx011xxx
                je Call_Indirect_Far
                cmp ecx, 100b
                je Jmp_Indirect_Near
                cmp ecx. 101b
                je Jmp_Indirect_Far
                ret ; other instructions
Call_Indirect_Near:
                mov ecx, esi
                sub ecx, ebx ; esi-ebx is command length
                add cx, R_IP ; add to R_IP to get offset of next instruction
                pushEmulatorStack cx ; push return address
Jmp_Indirect_Near:
                mov cx, word ptr [edx] ; load offset into cx
                mov R_IP, cx ; write back new ip
                jmp ControlTransfer_Done
Call_Indirect_Far:
                mov cx, R_CS
                pushEmulatorStack cx
                mov ecx, esi
                sub ecx, ebx
                add cx, R_IP
                pushEmulatorStack cx
Jmp_Indirect_Far:
                mov ecx, dword ptr [edx]
                mov R_IP, cx
                shr ecx, 16
                mov R_CS, cx
                jmp ControlTransfer_Done
ControlTransfer_Done:
                ret
ControlTransfer ENDP

; error case return address passed by ecx
; success will use ret
DataTransferMOV PROC
                computeFlatIP ; in ebx
                movzx eax, word ptr [ebx]
                xor al, 10001000b
                test al, 11111000b ; high 5 10001
                jz RegWithRegOrMem; 10001xxx
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
    RegWithRegOrMem:
                computeEffectiveAddress SrcIsRegOrImm, 0
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
                mov byte ptr REGB[ecx * 2 + ebx], 
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