# 说明文档

## 开发环境

masm32

ollydbg

vscode, git

## 功能实现

### 功能简介

实现了支持大部分常用指令的8086模拟器

完全支持指令（不支持前缀，如Segment Override Prefix）

+ ADD, OR, ADC, SBB, AND, SUB, XOR, CMP
+ INC, DEC
+ MOV
+ XCHG
+ PUSH, POP
+ CALL, JMP, RET, Jcc(All Conditional Jump Instructions)
+ CLC, STC
+ HLT

尚未实现的重要指令

+ INT, INTO
+ IN, OUT
+ TEST
+ SAHF, LAHF
+ NOT, NEG, MUL, IMUL, DIV, IDIV
+ SHL/SAL(same op), SHR, SAR
+ ROL, ROR, RCL, RCR
+ LEA

### 实现原理与难点

由于现代x86/x86_64处理器支持的指令是8086的超集，指令的实现较为简单。

困难之处在于指令的解码、尤其是高效的解码，如何减少内存访问次数、减少数据传送次数（尽管现代处理器可通过register renaming实现zero latency reg to reg mov）、减少跳转次数、减少处理器在关键路径上的Stall周期数、减少关键路径的周期数。同时还需作出取舍，牺牲部分性能换取更高的代码可复用性和更小的二进制文件。

为了实现高效解码，我们采取了多种策略

+ 用位运算计算索引代替条件跳转和读取，消除了不可预测的分支
  + 这需要合适的变量布局配合
+ 用xor+test代替and+cmp实现部分位匹配，从而只需读取一次数据便可进行多次匹配
+ 用位运算+跳转表代替条件跳转