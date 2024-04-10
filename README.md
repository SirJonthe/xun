# XUN

## Copyright
2024

github.com/SirJonthe

## About

`XUN` is a fictional 16-bit computing platform.

## TODO

// [ ] xasm: Remember to use xcc_push_scope and xcc_pop_scope even for program scope
// [ ] xb:   Remember to use xcc_push_scope and xcc_pop_scope even for program scope
// [ ] xun:  Instructions and library functions to detect hardware and send and receive data from ports
// [ ] xb:   Arrays without explicit size
// [ ] xb:   static (variables stored in binary, RLA used to address)
// [ ] xb:   signed/unsigned
// [ ] xb:   ++*ptr
// [ ] xb:   argc, argv
// [ ] xb:   namespace
// [ ] xcc:  Move file loading to XCC, and implement water-tight logic for preventing including the same file twice

## Design

`XUN` is a 16-bit fictional computing platform, meaning that it is a virtual machine, or emulator, for the XUN instruction set architecture. The user can write programs, compile them, and run them inside the virtual machine under, admittedly, arbitrary constraints.

`XUN` comes with necessary tools, such as compilers, standard libraries, and debuggers, to build programs. There are two programming languates available; XASM, and XB. XASM is an assembly language which assembles into the XUN instruction set and closely mirrors how the XUN processor works (with some syntactic sugar). XB is a variant of Ken Thompson's B programming language (the typeless predecessor to C) with some improvements from C (such as the ability to declare variables wherever needed) and some omissions (such as `goto` and `switch-case`).

`XUN` does not include platform-specific input/output integrations to keep the core portable. As such, the user will need to implement this themselves (extending `Device` is a good idea for this purpose).

## Manuals

`XUN` contains manuals that are written in a way that combines the technical specification of the thing it documents, but also includes fiction surrounding motivations, background, etc. in order to construct a convincing fiction.

## Tools
### XCC

XCC (XERXES Compiler Collection) is the back-end that is used for both the XASM and the XB programming languages to ensure interoperability of the output binaries. The back end mainly consists of breaking out common functionality between both languages. This allows for such things as the same program being compiled in all languages that use XCC as the compiler back-end.

See Programming Manual for XERXES(tm) Unified Nanocontroller Assembly Language (XASM) for technical details about the programming language.

See Programming Manual for XERXES(tm) B (XB) for technical details about the programming language.


## Building

## Examples
