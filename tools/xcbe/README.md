XUN Compiler BackEnd

Refactor xasm and xcc in such a way that the common elements of xasm and xcc are spun off into a backend. That way the backend object can be passed from one compiler to the next, compiling parts of a program in one language and other parts in another.

This will allow for inline assembly in C without the need for re-implementing the entire XASM language inside the C compiler.
