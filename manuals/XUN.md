# The XERXES(tm) Unified Nanocontroller Technical Manual

## Introduction

## The XUN Instruction Set Architecture (XIS)

[instruction names, opcodes, and brief descriptions here]

## Memory layout
Memory allocated on the stack is ordered to higher memory addresses in sequence of order.

## Registers

### Stack pointer register

### Intruction pointer register

### Instruction register

### I/O register

### Offset registers
The XUN architecture provides three offset registers

### Error register

## Input and Output

### Hardware
XUN is capable of communicating with devices that has completed the XERXES(tm) Official Certification Program. Any such device is granted a hardware ID which corresponds to the state of a counter that is incremented by one at the time of each successful completion of the XERXES(tm) Official Certification Program. The hardware ID number `0xffff` is reserved for XUN.

Please note that hardware that has not completed the XERXES(tm) Official Certification Program is to be considered unsupported, potentially malicious, and may provide hardware ID:s that collide with existing, officially certified hardware resulting in unexpected behavior. Always carefully inspect hardware before plugging it into XUN. If you suspect a device in your possession is unlicensed, please contact the device retailer for clarification and for a potential refund.

### Communication protocol
XUN has no support for raw bitwise input and output. Instead, the XUN architecture mandates that all compatible devices communicate using data packets formatted in accordance with the XERXES(tm) Device Communication Packet Specification.

The specification is as follows:
