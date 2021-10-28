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
                push ax ; op[3] in first byte, but we only can push 16bit reg
                shr eax, 8 ; prepare for ModeDecode
                jmp ModeDecode
DecodeIRM:
                shr eax, 8 ; op[3] in second byte
                push ax
                ; fall-through
ModeDecode:
                ; eax already = mod[2] reg[3] r/m[3]
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
                jnz Exec ; Imm to r/m, no need to exchange
                test edi, 0010b; d[1]
                jz Exec ; d = 0 no need to exchange
                xchg esi, edx ; put src in esi and dest in edx, for sub/sbb/cmp and write back
                ; fall-through
Exec:           
                ; first byte still in edi
                pop cx ; decode op
                and ecx, 00111000b
                shr ecx, 1 ; Not fully shift, eliminate * 4 for sizeof dword in OpTable
                mov eax, dword ptr [OpTable + ecx]
                ; prepare operand, using movzx
                test edi, 0001b ; decide 8bit or 16bit operand
                jnz OperandW ; word operand
                movzx ecx, byte ptr [edx] ; dest operand
                movzx esi, byte ptr [esi] ; src operand, no need to preserve src addr
                jmp eax
OperandW:
                ; first byte still in edi
                movzx ecx, word ptr [edx]
                test edi, 10000000b
                jz NotSignExt
                test edi, 0010b; s[1]
                jz NotSignExt
                movsx esi, byte ptr [esi]
                jmp eax
NotSignExt:
                movzx esi, word ptr [esi]
                jmp eax
OpTable:
                ; could store diff to some near Anchor(e.g. OpTable) to save space
                ; but we use a straightforward method
                dword I_ADD
                dword I_OR
                dword I_ADC
                dword I_SBB
I_ADD:
                add ecx, esi
                jmp WriteBack
I_OR:
                or ecx, esi
                jmp WriteBack
I_ADC:
                adc ecx, esi
                jmp WriteBack
I_SBB:
                sbb ecx, esi
                jmp WriteBack
I_AND:
                and ecx, esi
                jmp WriteBack
I_SUB:
                sub ecx, esi
                jmp WriteBack
I_XOR:
                xor ecx, esi
                ; fall through
WriteBack:
                test edi, 0001b ; decide 8bit or 16bit operand
                jnz WriteBackW
                mov byte ptr [edx], cl
                ret
WriteBackW:
                mov word ptr [edx], cx
                ret
I_CMP:
                cmp edi, ecx
                lahf
                mov R_FLAGS, ah
                ret
ArithLogic ENDP
run:
                invoke printf, offset szMsg, eax, ebx
                ret
end				run