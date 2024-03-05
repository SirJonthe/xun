# Note
These libraries are an implementation of some of the standard C library for use with the `xcc` C compiler. When building your application you generally do not want to include these files in the build as you already have a better implementation available to you.

However, once you have a build of `xcc` you will want to ship this C library implementation with it and ensure that it can find the files so that `xcc` can be properly used to compile C programs into 16 bit XUN programs.

# TODO
[ ] We write the libc runtime here (for use by the custom C compiler).