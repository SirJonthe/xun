# Programming Manual for XERXES(tm) B (XB)

## Introduction

## Build system
XB does not come with an explicit build system. However, the general way of building is to explicitly supply the compiler with the source files (not headers) to compile.

Source files are files that contain definitions for local and global symbols, and are usually denoted by the file extension `xb`. Header files are files that contain declarations and definitions of global symbols, and are usually denoted by the file extension `xh`.

## Statements
### Types
[what types of statements are there?]

### Global scope
[what types of statements are valid in the global scope?]

### Local scope
[what types of statements are valid in the local scope?]

## Literals
All literals in XB are unsigned integers.

Literals are bare numbers, such as `1`, `0xff00`, and `'a'`. There are two main types of literals; integers and characters. Integers can be expressed in both decimal (default, no prefix) and hexadecimal (requires `0x` prefix on the literal). Characters, which still evaluate to unsigned integers, are expressed as a single character enclosed in a `'` character.

Strings are not considered a third form of literal as strings are syntactic sugar for arrays of characters.

## Variables
All variables in XB are unsigned integers and are declared and/or defined using the `auto` keyword, indicating automatic storage duration which collapses the stack when the variable falls out of scope.
```
auto a = 10;        // 'a' is 10.
auto b = a * 3 + 5; // 'b' is 35.
auto c;             // 'c' is undefined.
```

Several variables can be declared and/or defined in the same statement:
```
auto a = 10, b = a * 3 + 5, c; // Generates the same code as the previous code snippet.
```

## Arrays

## Strings
Strings are any number of characters enclosed between two `"` characters. Under the hood, strings are a form of syntactic sugar for an array of characters, where each character in the string represents an element in the array.
```
auto str[13] = "Hello, World!";
```

Strings, however, have one defining difference from arrays; Strings consist of a single additional character than what was specified. In the example above, the string is actually 14 characters long. The added character is appended to the end of the string and has the value `0` which is used to determine the end of the string.

## Constants
Constants in XB are literals. As a compile-time constant, no operand in an expression used to set the value of a constant may be a value that can not be determined at compile-time.
```
const a = 10;      // OK: 10 is a literal.
const b = a + 100; // OK: Both 'a' and 100 are literals.
auto  c = 1;
const d = c;       // Error: 'c' is not a compile-time constant.
```

Constants may be defined as any complex expression using only any operand that can be determined at compile-time.

Arrays can not be constants, but variable arrays can still be defined using constants.

## Shadowing
XB allows the programmer to declare aliases with the same name as aliases declared in previous scopes (not in the same scope). This results in alias shadowing, meaning that the following code is valid:
```
auto x = 0;
{
	auto x = 123;
}
```

The context determines what alias is accessed.
```
auto x = 0;
{
	auto x = 123;
	if (x == 123) {
		// True since 'x' refers to a variable with the 123 value.
	}
}
if (x == 0) {
	// True since 'x' refers to a variable with the 0 value.
}
```

Whenever an alias is shadowed the the alias declared in the topmost scope is accessed. The alias search can be reversed using the `::` operator.

```
auto x = 0;
{
	auto x = 123;
	if (::x == 0) {
		// True since 'x' refers to a variable with the 0 value.
	}
}
if (x == 0) {
	// True since 'x' refers to a variable with the 0 value.
}
```

The same logic applies to all aliasable language elements, such as variables, constants, functions, enums, etc.

## Standard library
XB comes with a small set of essential tools for writing applcations and utilizing XUN hardware.