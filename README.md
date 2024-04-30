# XUN

## Copyright
2024

github.com/SirJonthe

## About

`XUN` is a fictional 16-bit computing platform.

## TODO

[ ] xun:  Internal devices need to cycle so that they can react and clear message queue
[ ] xb:   Create translation units by clearing names of symbols declared in source files when finishing compilation of that source file.
[ ] xb:   Better error messages across separate files.
[ ] xasm: Remember to use xcc_push_scope and xcc_pop_scope even for program scope
[ ] xb:   Remember to use xcc_push_scope and xcc_pop_scope even for program scope
[ ] xb:   Arrays without explicit size
[ ] xb:   static (variables stored in binary, RLA used to address)
[ ] xcc:  Deprecate HALT, and use LDA
[ ] xasm: Deprecate HALT
[ ] xb:   signed/unsigned
[ ] xb:   ++*ptr
[ ] xb:   argc, argv
[ ] xb:   robust function call: fn[index1](param1, param2)[index2](param2, param3) etc...
[ ] xb:   namespace

## Design

`XUN` (XERXES Unified Nanocontroller) is a 16-bit fictional computing platform, meaning that it is a virtual machine, or emulator, for the XUN instruction set architecture. The user can write programs, compile them, and run them inside the virtual machine under, admittedly, arbitrary constraints.

`XUN` comes with necessary tools, such as compilers, standard libraries, and debuggers, to build programs. There are two programming languates available; XASM, and XB. XASM is an assembly language which assembles into the XUN instruction set and closely mirrors how the XUN processor works (with some syntactic sugar). XB is a variant of Ken Thompson's B programming language with some omissions from the spec and additions from C.

`XUN` does not include platform-specific input/output integrations to keep the core portable. As such, the user will need to implement this themselves (extending `Device` is a good idea for this purpose).

## Manuals

`XUN` contains manuals that are written in a way that combines the technical specification of the thing it documents, but also includes fiction surrounding motivations, background, etc. of technical decisions in order to construct convincing fiction.

## Tools

### Introduction

The included tools are mainly aimed at development of programs for XUN as well as operation of the system.

There are two types of tools; External tools which are not self-hosted on the XUN platform due to memory limitations or otherwise limits general usability, and internal tools which are self-hosted on the XUN platform and used to operate the XUN system.

### External tools

#### XCC

XCC (XERXES Compiler Collection) is the back-end that is used for both the XASM and the XB programming languages to ensure interoperability of the output binaries. The back-end mainly consists of breaking out common functionality between both languages. This allows for programs to be written in both XASM and XB without the two languages generating incompatible binary output.

#### XASM

XASM (XERXES Assembly Language) is an external that is a low-overhead assembler that allows the programmer to access most instructions of the XUN instruction set in a 1:1 manner with some exceptions to make the code more readable and the effects more predictable and in-line with established programming languages. There are only two instructions that take parameters; `put`, which places a value on the stack, and `mov` which pops the top value off the stack and stores it in the specified address. All other instructions work by operating on values on the stack and are free to add, remove, or just read values on the stack.

XASM adds some meta-programming, i.e. programming of the assembler, which allows for such things as adding symbols to the assembler, emitting binary data to the output, and accessing registers (which are actually hidden behind separate instructions in the instruction set, but accessed using assembler directives to more closely resemble symbols).

XASM does not come with a standard library. It is up to the programmer to construct the tools necessary for the use-case.

See Programming Manual for XERXES(tm) Unified Nanocontroller Assembly Language (XASM) for technical details about the programming language.

#### XB

XB (XERXES B) an external tool that is a flavor of Ken Thompson's B (the predecessor of C) that is intended for more high-level programming and increased productivity. On a small computer platform such as XUN, even a low-overhead programming language such as XB may introduce significant overhead in terms of binary size and performance. However, it may still be a much preferrable option since XASM may be quite verbose, hard to read, and error prone.

XB supports most of the convienience of C, such as functions, return values, expressions, control flow, array indexing, pointer indirection, etc.

XB has no types. All variables and constants are unsigned integers. Pointers are unsigned integers as well, and it is up to the programmer to distinguish between use cases.

XB comes with a standard library that mirrors that of commonly used C standard library functionality. It also includes non-standard functionality that can be used to access system features, such as device I/O.

See Programming Manual for XERXES(tm) B (XB) for technical details about the programming language.

#### XDB

XDB (XERXES Debugger) an external tool that is a binary debugger that executes a binary step by step. The debugger has an option for a terminal-based UI where the used can see the program, stack, and register values as the program executes.

## Building

## Examples

## Lessons learned

### Why no in-place arrays in C as function parameters?

Not being able to call a function with in-place arrays in C has always been irritating:

```
fn({1, 2, 3}); // error
```

However, in XB, declaring an array like the one above emits four values; a pointer to address of 1 (the first element in the array), then 1, 2, and 3. C does this as well. This means that, at the call site above, these four values are put on the top of the stack (the array pointer and three array values) when the function is really only expecting one value on the stack (the array pointer). Making the code above work means we need to create an exception for how code is generated for in-place arrays when used as function parameters leading to an increased complexity of the compiler.

The reason string literals can be used as in-place parameters in C...

```
fn("String"); // no error
```

... is because string literals in C are stored in a global variable space rather than in a local variable space. This is the reason why two identical string literals across different pieces of code results (most often) in the same string pointer - The string literal pointer points to the same global variable space. In C, the only thing emitted at the function call site, where the parameters should be, is the string pointer, not the whole string. XB emits strings locally just like normal arrays, meaning these kinds of function calls do not work for the same reason as in-place array parameters do not work as detailed above.

### Unoptimized low-level programming languages add a lot of overhead to programs

Even low-level programming languages such as C adds an inordinate amount of overhead to programs unless optimized by the compiler. Each low-level language construct such as function calls, control flow, and even expressions adds many instructions to the binary which could, in some cases, be omitted in context or be made much more efficient in assembly. For modern compilers this is not a problem due to optimization which makes the output binary fall more in line with hand-optimized assembly, but since XB does not come with an optimization step the overhead can be quite noticeable over XASM.