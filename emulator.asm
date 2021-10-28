.686
.model flat, stdcall
option casemap:none

includelib      msvcrt.lib
printf          PROTO C :ptr byte, :VARARG

.data
szMsg			byte "%d %d", 0AH, 0DH, 0
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

MEMO            byte 1048576 DUP(?)
MEMO_Guard      byte 00FFH

.code
ArithLogic PROC, ip: ptr byte
                mov ebx, ip
                movzx eax, word ptr [ebx]; read 2 bytes at once for later use, may exceed 1M, but we are in a emulator
                test eax, 11000100b ; only test low byte -- the first byte
                jz DecodeRRM; must be 00xxx0xx, No Imm Op
                test eax, 01111100b
                jz DecodeIRM; must be 100000xx(with test 11000100b not zero), Imm to reg/mem
                test eax, 11000010b
                jz DecodeIAC; must be 00xxx10x(with test 11000100b not zero), Imm to accumulator Op
                ret; Other Instructions
DecodeRRM:
                push eax ; op[3] in first byte, but we push 32bit to prevent partial register stalls when pop
                shr eax, 8 ; prepare for ModeDecode
                jmp ModeDecode
DecodeIRM:
                shr eax, 8 ; op[3] in second byte
                push eax
                ; fall-through
ModeDecode:
                ; eax low8 already = mod[2] reg[3] r/m[3] or mod[2] op[3] r/m[3]
                mov ecx, eax
                shr ecx, 6; mod
                jnz MOD123
                ; mod = 00
                mov ecx, eax ; mod[2] reg[3] r/m[3]
                and ecx, 0111b ; r/m[3]
                cmp ecx, 0110b ; check special case
                jne NO_DISP
                ; r/m = 110 special case, displacement only
                movzx edi, word ptr [ebx + 2]
                lea esi, [ebx + 4] ; Start of Imm data
                jmp ADD_DISP
NO_DISP:
                xor edi, edi ; common case, no displacement
                lea esi, [ebx + 2] ; Start of Imm data
                jmp E_ADDR
MOD123:
                cmp ecx, 1
                jne MOD23
                ; mod = 01
                movzx edi, byte ptr [ebx + 2]
                lea esi, [ebx + 3] ; Start of Imm data
                jmp E_ADDR
MOD23:
                cmp ecx, 2
                jne MOD3
                ; mod = 10
                movzx edi, word ptr [ebx + 2]
                lea esi, [ebx + 4] ; Start of Imm data
                ; fall-through E_ADDR
E_ADDR:
                ; displacement in edi
                mov ecx, eax ; mod[2] reg[3] r/m[3]
                test ecx, 0100b
                jnz RM1XX
                ; r/m = 0xx
                and ecx, 0010b ; Base = b ? BP : BX, R_BP = R_BX + 4
                movzx edx, word ptr R_BX[ecx * 2] ; actually word ptr is not needed
                mov ecx, eax ; mod[2] reg[3] r/m[3]
                and ecx, 0001b ; Index = i ? DI : SI, R_DI = R_SI + 4
                movzx ecx, word ptr R_SI[ecx * 2]
                add edx, ecx
                jmp ADD_DISP
RM1XX:
                test ecx, 0001b
                jnz RM11X
                ; r/m = 10x
                and ecx, 0001b ; Index = i ? DI : SI, R_DI = R_SI + 4
                movzx edx, word ptr R_SI[ecx * 2]
                jmp ADD_DISP
RM11X
                ; r/m = 11x
                and ecx, 0001b ; Base = b ? BP : BX, R_BP = R_BX + 4
                movzx edx, word ptr R_BX[ecx * 4]
                jmp ADD_DISP
ADD_DISP:
                add edx, edi ; (virtual) effective address now in edx
                ; now edi free
                add edx, offset MEMO
                movzx ecx, R_DS ; (virtual) data segment, may be override, TODO
                add edx, ecx
                movzx edi, byte ptr [ebx] ; prepare for REG_OR_IMM
                jmp REG_OR_IMM
MOD3:
                ; r/m = register
                movzx edi, byte ptr [ebx]
                mov ecx, eax ; mod[2] reg[3] r/m[3], moved before jump to reuse code
                test edi, 0001b ; decide 16bit or 8bit register
                jnz RM_REGW
                ; 8bit register
                and ecx, 0011b
                lea edx, REGB[ecx * 2] ; ACDB
                mov ecx, eax
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add edx, ecx ; register "address" now in edx
                jmp REG_OR_IMM
RM_REGW:
                ; 16bit register
                and ecx, 0111b
                lea edx, REGW[ecx * 2] ; register "address" now in edx
                ; fall-through REG_OR_IMM
REG_OR_IMM:
                test edi, 10000000b ; first byte already in edi
                jnz IMM_SRC
                ; Not Imm, Use Reg
                shr eax, 3 
                mov ecx, eax ; 000 mod[2] reg[3], moved before jump to reuse code
                test edi, 0001b ; decide 16bit or 8bit register
                jnz REG_REGW
                ; 8bit register
                and ecx, 0011b ; ecx = 000 mod[2] reg[3]
                lea esi, REGB[ecx * 2] ; ACDB
                mov ecx, eax
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add esi, ecx ; reg register "address" now in esi
                jmp SRC_DEST ; first byte still in edi
REG_REGW:
                ; 16bit register
                and ecx, 0111b
                lea esi, REGW[ecx * 2] ; reg register "address" now in esi
                jmp SRC_DEST ; first byte still in edi
IMM_SRC:
                ; (virtual) imm data address already in esi
                add esi, offset MEMO
                movzx ecx, word ptr R_CS
                add esi, ecx
                jmp SRC_DEST ; first byte still in edi
DecodeIAC:

SRC_DEST:
                ; first byte already in edi
                test edi, 10000000b;
                jnz OperandB ; Imm to r/m, no need to exchange
                test edi, 0010b; d[1] or s[1] (Imm to r/m case)
                jz OperandB ; d = 0 no need to exchange
                xchg esi, edx ; put src in esi and dest in edx, for sub/sbb/cmp and write back
                ; fall-through
OperandB:           
                ; first byte still in edi
                test edi, 0001b ; decide 8bit or 16bit operand
                jnz OperandW ; word operand
                ; use 8bit partia reg for convenience
                ; generally that will be slower because of partial register stalls
                ; fortunately we don't need to read from cx or ecx, actually no stall occur
                mov cl, byte ptr [edx] ; dest operand
                mov ch, byte ptr [esi] ; src operand
                pop eax ; decode op
                and eax, 00111000b ; select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + eax]
OperandW:
                ; first byte still in edi
                movzx ecx, word ptr [edx]; only use cx
                test edi, 10000000b
                jz NotSignExt ; No Imm
                test edi, 0010b; s[1]
                jz NotSignExt
                movsx si, byte ptr [esi] ; src operand, no need to preserve its addr
                jmp OperandWExec
NotSignExt:
                mov si, word ptr [esi]
                ; fall-through
OperandWExec:
                pop eax ; decode op
                and eax, 00111000b ; select bits, clear others
                ; Not shift, eliminate index * 8 for OpTable
                jmp dword ptr [OpTable + 4 + eax]
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
                sbb cx, ch
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
ArithLogic ENDP
run:
                invoke printf, offset szMsg, eax, ebx
                ret
end				run