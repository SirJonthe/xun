# XERXES(tm) Compiler Collection

## Intro

This is the back end that is used for both the XERXES(tm) Unified Nanocontroller Assembly Language (XASM) and the XUN B (XB) programming languages to ensure interoperability in the form of inline assembly in XB. The back end mainly consists of breaking out common functionality between both languages.

## XERXES(tm) Unified Nanocontroller Assembly Language (XASM)

See Programming Manual for XERXES(tm) Unified Nanocontroller Assembly Language (XASM) for technical details about the programming language.

## XERXES(tm) B Programming Language (XB)

See Programming Manual for XERXES(tm) B (XB) for technical details about the programming language.

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
