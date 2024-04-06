# XUN Compiler Collection

## Intro

This is the back end that is used for both the XASM and the XB programming languages to ensure interoperability in the form of inline assembly in XB. The back end mainly consists of breaking out common functionality between both languages.

## TODO

// [ ] xasm: XASM to use write_rel.
// [ ] xasm: Prevent emitting "bin", "lit", "here", etc. as instructions in try_instruction_all
// [ ] xun:  Instructions and library functions to detect hardware and send and receive data from ports
// [ ] xb:   Arrays without explicit size
// [ ] xb:   Include files (hard because it requires a virtual file system)
// [ ] xb:   signed/unsigned
// [ ] xb:   static (variables stored in binary, RLA used to address)
// [ ] xb:   ++*ptr
// [ ] xb:   namespace
// [ ] xb:   Re-assignments as expression statements: a = b = c;