# Programming Manual for XERXES(tm) B (XB)

## Introduction

## Build system

XB does not come with an explicit build system. However, the general way of building is to explicitly supply the compiler with the source files (not headers) to compile.

Source files are files that contain definitions for local and global symbols, and are usually denoted by the file extension `xb`. Header files are files that contain declarations and definitions of global symbols, and are usually denoted by the file extension `xh`.