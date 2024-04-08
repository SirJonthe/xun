# XUN Compiler Collection

## Intro

This is the back end that is used for both the XASM and the XB programming languages to ensure interoperability in the form of inline assembly in XB. The back end mainly consists of breaking out common functionality between both languages.

## TODO

// [ ] xb:   Fix: fn(); auto x = fn(); fn() { return 0xfefe }
//           Solution: In sym->link store IP of PUT 0 (declaration). When fn is defined, store IP in RAM[sym->link], and set sym->link to 0
//           Solution: Mirror this solution for labels in XASM
// [ ] xasm: Remember to use xcc_push_scope and xcc_pop_scope even for program scope
// [ ] xb:   Remember to use xcc_push_scope and xcc_pop_scope even for program scope
// [ ] xasm: Labels
// [ ] xun:  Instructions and library functions to detect hardware and send and receive data from ports
// [ ] xb:   Arrays without explicit size
// [ ] xb:   Include files (hard because it requires a virtual file system)
// [ ] xb:   static (variables stored in binary, RLA used to address)
// [ ] xb:   signed/unsigned
// [ ] xb:   ++*ptr
// [ ] xb:   namespace
