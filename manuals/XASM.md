# Programming Manual for XERXES(tm) Unified Nanocontroller Assembly Language (XASM)

## Introduction
### A note on code listings
Note that `...` may be used in code listings to indicate zero or more non-specific statements. XASM does not compile `...` as an instruction, so it is used purely for illustrative purposes.

## Instructions

### Stack manipulation

### Intruction pointer manipulation

#### Labels
A label is declared as follows:
```
%label:
```
A label can be jumped to as follows:
```
put %label.
jmp.
```
* Note that any instruction used to manipulate the instruction pointer can be used in stead of `jmp` in the example above.

Labels can be used before they are declared.
```
put %label.
jmp.
%label:
```

Labels can not be used to jump out of the current scope, and fall out of scope the same way as variables and constants.
```
%label1:
$scope: {
	%label2:

	put %label1. // ERR: $label1 out of current scope.
	jmp.

	put %label3. // ERR: %label3 not linked.
	jmp.
}
%label3:
put %label2. // ERR: %label2 no longer available.
jmp.
```

Similarly, labels are only

### Arithmetic

### Logic

### I/O
