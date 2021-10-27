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


.code
ArithLogic PROC, ip: ptr byte
                mov ebx, ip
                movzx eax, byte ptr [ebx]
                test eax, 11000100b
                jz DecodeRRM; must be 00xxx0xx, No Imm Op
                test eax, 01111100b
                jz DecodeIRM; must be 100000xx(with test 11000100b not zero), Imm to reg/mem
                test eax, 11000010b
                jz DecodeIAC; must be 00xxx10x(with test 11000100b not zero), Imm to accumulator Op
                ret; Other Instructions
DecodeRRM:
DecodeIRM:
                movzx eax, byte ptr [ebx + 1]; mod[2] reg[3] r/m[3]
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
                add edx, edi ; effective address now in edx
                jmp REG_OR_IMM
MOD3:
                ; r/m = register
                ; need to decide 16bit or 8bit register
                movzx ecx, byte ptr [ebx]
                test ecx, 0001b
                jnz RM_REGW
                ; 8bit register
                mov ecx, eax ; mod[2] reg[3] r/m[3]
                and ecx, 0011b
                lea edx, REGB[ecx * 2] ; ACDB
                mov ecx, eax
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add edx, ecx ; register "address" now in edx
                jmp REG_OR_IMM
RM_REGW:
                ; 16bit register
                mov ecx, eax
                and ecx, 0111b
                lea edx, REGW[ecx * 2] ; register "address" now in edx
                ; fall-through REG_OR_IMM
REG_OR_IMM:
                movzx ecx, byte ptr [ebx]
                test ecx, 10000000b
                jnz IMM_SRC
                ; Not Imm, Use Reg
                shr eax, 3 ; 000 mod[2] reg[3]
                ; need to decide 16bit or 8bit register
                test ecx, 0001b
                jnz REG_REGW
                ; 8bit register
                mov ecx, eax ; 000 mod[2] reg[3]
                and ecx, 0011b
                lea esi, REGB[ecx * 2] ; ACDB
                mov ecx, eax
                and ecx, 0100b ; 0 -> L, 1 -> H
                shr ecx, 2
                add esi, ecx ; reg register "address" now in esi
                jmp Exec
REG_REGW:
                ; 16bit register
                mov ecx, eax
                and ecx, 0111b
                lea esi, REGW[ecx * 2] ; reg register "address" now in esi
                jmp Exec
IMM_SRC:
                ; imm data address already in esi
DecodeIAC:

Exec:
                mov edx, OpTable
                ;mov edi, i
                movzx eax, word ptr [edx + edi * 2]
                add eax, edx
                jmp eax
OpTable:
                word I_ADD - OpTable
                word I_OR - OpTable
                word I_ADC - OpTable
                word I_SBB - OpTable
I_ADD:
                invoke printf, offset szMsg, edi
                ret
I_OR:
                invoke printf, offset szMsg, edi
                ret
I_ADC:
                invoke printf, offset szMsg, edi
                ret
I_SBB:
                invoke printf, offset szMsg, edi
                ret                      
ArithLogic ENDP
run:
                invoke printf, offset szMsg, eax, ebx
                ret
end				run