XUN
===

Legal notice
------------
Copyright Jonathan Karlsson 2017

* This library is free to use, share, and modify for anyone, as long
  as credit goes where credit is due. In short, have the decency to
  credit the original author (and subsequent authors in case of modification).
* This code is provided "as-is", i.e. the author give no guarantees as to how
  well it will work, if at all, for whatever purpose.
* The author of this code takes no responsibility for any damages
  resulting directly or indirectly from the use of this code.

About
-----
This project includes a small virtual machine capable of addressing and computing 16-bit numbers.
It supports external hardware as long as a device driver is written for the virtual machine (see Device base class).
A basic video driver is included that uses SDL for output.
A driver for a clock component is included.

Dependencies
------------
* MiniLib for utilities
* SDL for testing

Compiling
---------
* The project itself should be able to be compiled without any added settings to the compiler.
* Setting up SDL will vary from OS to OS and compiler to compiler.

NOTES
-----
* This software should be considered in alpha stage.
* This library is still under construction. Current design
  may be subject to change.
* Missing or functionality that does not behave as expected
  is a very real risk.

