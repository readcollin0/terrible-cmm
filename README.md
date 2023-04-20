# Terrible-CMM
A C-- compiler for RISC-V

I have never written a compiler before.
I have never studied compilers.
I have never written a big program in Common Lisp.

And so, I decided to write a compiler in Common Lisp. It's built to target specifically the RISC-V processor built in my EC EN 323 class at BYU, which doesn't implement all of the instructions. I also didn't want to write a full C compiler, as that's too much work for the weekend I had to do it. And so, I made C--. 

Look at the source at your own risk. I don't know how to write good Lisp code.
Learn the language at your own. I don't have experience in language design, or compilers, so I had to make it easy to compile.

Documentation (probably) coming soon.



## Requirements
 - [SBCL](https://www.sbcl.org/)
 - [QuickLisp](https://www.quicklisp.org/beta/)

## Notes
 - Compiler options are not currently supported on the native command line. The `full-compile` function has keyword argument to pass options. You can take a look at those if you want.
 - The folders are *very* loosely named.
   - The `lib` directory contains several useful functions, most of which are specifically for the processor we built, and not really useful elsewhere. Most of the functions are actually interesting to look at.
   - The `test` directory is mostly just some files that I used to make sure functionality was working, and *not* anything like formal tests. The code there is simply meant to be compiled for me.
   - The `demo` directory has some pretty generic stuff to see the language work. Unfortunately, most of the recent development was on the game directory, and I have modified the compiler without testing this directory, so it may not compile anymore. Most of the code works, though.
   - The `game` directory is the work to make something usable on the FPGA. It is not currently a game of any sort. It *definitely can* be compiled, however.
