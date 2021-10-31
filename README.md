# 8086

[中文版](./README.zh.md)

## Who's responsible for ...?

+ @panda2134：

  CALL, JMP, RET, Jcc, STC, CLC, HLT

  TUI Interface, Video Memory Rendering

  Loading executable

  Test programs

  Debugging with OllyDbg

+ @YouJiacheng：

  Effective Address Computation along with its unit tests

  ADD, OR, ADC, SBB, AND, SUB, XOR, CMP

  INC, DEC, MOV, XCHG, PUSH, POP

  Optimization

  Code Review

  Collecting documents on 8086 machine code

## Showcase

- Calculation test from [CodeGolf](https://codegolf.stackexchange.com/questions/4732/emulate-an-intel-8086-cpu) , with modifications to video memory layouts.
  ![test](./docs/test.png)
- Video memory test on the emulated CGA 80x25 display.
  ![color](./docs/color.png)

## Develop Environment

- Windows 10
- MASM32 
- OllyDbg
- Visual Studio Code, Git

