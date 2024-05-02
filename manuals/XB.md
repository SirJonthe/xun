# Programming Manual for XERXES(tm) B (XB)

## Introduction

## Build system
Source code in XB is generally divided into two types of files; Source files and header files. Source files are files that contain definitions for local and global symbols, and are usually denoted by the file extension `xb`. Header files are files that contain declarations and definitions of global symbols, and are usually denoted by the file extension `xh`.

XB does not come with an explicit build system. However, the general way of building is to explicitly supply the compiler with the source files (not headers) to compile. Headers are generally not needed to supply to the compiler since the source code in the source files directly references the header files for the compiler to look up via `#include` directives.
```
xb file01.xb ... filenn.xb
```

## Statements
### Types
[what types of statements are there?]

### Global scope
[what types of statements are valid in the global scope?]

### Local scope
[what types of statements are valid in the local scope?]

## Literals
All literals in XB are unsigned integers.

Literals are bare numbers, such as `1`, `0xff00`, and `'a'`. There are two main types of literals; Integers and characters. Integers can be expressed in both decimal (default, no prefix) and hexadecimal (requires `0x` prefix on the literal). Characters, which still evaluate to unsigned integers, are expressed as a single character enclosed between two `'` characters.

Strings, which will be covered later, are not considered a third form of literal as strings are syntactic sugar for arrays of characters.

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

Strings, however, have one defining difference from arrays; Strings consist of a single additional character than what was specified. In the example above, the string is actually 14 characters long. The added character is appended to the end of the string and has the value `0` which is used to determine the end of the string when treated as a stream.

## Constants
Constants in XB are aliases for literals or other constants. Constants provide programmers with a better alternative to literals used more than once throughout the code and could be subject to change.

Constants may be defined as any complex expression using only operands that can be determined at compile-time.
```
const A = 10;      // OK: 10 is a literal.
const B = A + 100; // OK: Both 'A' and 100 are literals.
auto  c = 1;       // OK: Variables can be defined using constants and literals.
const D = c;       // Error: 'c' is not a compile-time constant.
```

Arrays can not be constants due to the issue of indexing them at run-time using variable indices without storing them as distinct memory locations inside the binary, but variable arrays can still be defined using constants.
```
const A = 10;
const B = A + 100;
const C = 1;

const D[3] = { A, B, C }; // Error: 'D' can not be indexed at run-time using variables without breaking requirement that 'D' is compile-time constant.

auto d[3] = { A, B, C }; // OK: 'd' is defined using constants.
```

## Functions

### Definitions

Optionally, for functions taking no arguments the programmer may specify that the function takes a single `void` parameter (this can aid reading, especially for function declarations).
```
fn1() // OK: Function takes no parameters.
{
	return 1;
}

fn2(void) // OK: Function takes no parameters.
{
	return 1;
}
```

### Declarations
Since functions can be called from other functions before they have been formally defined the compiler needs a way to know if the symbol used as a function makes up formally correct code.
```
fn2()
{
	return fn1(); // Error: The compiler does not know what 'fn1' is.
}

fn1()
{
	return 1;
}
```

This error can be solved via function declarations. As long as a function is declared before first use, the code is formally correct.
```
fn1(); // Declare function.

fn2()
{
	return fn1(); // OK: 'fn1' has already been declared
}

fn1() // Define function.
{
	return 1;
}
```

Take note that a function declaration and definition must align in function name and number of their parameters. However, there is no need for parameter names to match; Only their number must match.

It is a good idea to declare globally accessable functions inside header files rather than define them. That way the entire code is not exposed to other translation units, and also makes it possible to statically or dynamically link a binary against another binary containing the function definition.

Optionally, for functions taking no arguments the programmer may specify that the function takes a single `void` parameter. This also serves the purpose of making it clear that the programmer is referring to a function declaration and not a function call.
```
fn1();     // OK: Function takes no parameters, but looks identical to a function call.
fn2(void); // OK: Function takes no parameters, and can not be mistaken for a function call.
```

### Calls
Arrays can not be passed as parameters. Instead, pointers to arrays are passed to be dereferenced inside the function. This makes for a functionally similar way of accessing arrays as would passing arrays directly with the exception that the original data is accessable from within the function taking the array pointer as an argument, meaning the original array may be modified from within a function.

Any symbol may be used as a basis for a function call; Literals, constants, and variables may be used together with the function notation to call a function pointed to by the same.
```
// In global scope...
fn() {
	return 1;
}

auto fnptr = fn;
const FNPTR = 0x1000;

...

// In local scope...
fn();     // OK: The function is being called normally using the original function pointer.
fnptr();  // OK: Calls the function since 'fnptr' points to the same data as 'fn'. Not type safe.
FNPTR();  // OK: Uses the data at 0x1000 as the basis for a function call. Not type safe.
0x1000(); // OK: Uses the data at 0x1000 as the basis for a function call. Not type safe.
```

Note that calling functions using anything but the original function pointer loses the ability for the compiler to verify the correct number of input parameters at compile-time, so great care must be taken so that the stack is not corrupted by supplying the wrong number of parameters to a function pointer. Also, programmers must take very good care calling constants and literals as functions; The address supplied is absolute, so the programmer must be able to know for certain that a valid function is pointed to by the specified contant/literal.

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

## Include directives

## Standard library
XB comes with a small set of essential tools for writing applcations and utilizing XUN hardware.