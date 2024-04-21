# The XERXES(tm) Unified Nanocontroller Technical Manual

## Introduction

## The XUN Instruction Set Architecture (XIS)

## Memory layout

Memory allocated on the stack is ordered to higher memory addresses in sequence of order.

## Registers

### Stack pointer register

### Intruction pointer register

### Instruction register

### I/O register

### Offset registers

### Error register

## Input and Output

XUN has no support for raw bitwise input and output. Instead, the XUN architecture mandates that all compatible devices communicate using data packets formatted in accordance with the XERXES(tm) packet specification.

The specification is as follows: