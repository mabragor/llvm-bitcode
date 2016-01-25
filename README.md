llvm-bitcode
------------

LLVM IR is a very cool thing. But to generate it using LLVM's C (or C++) API
is a total pain (even using CL-LLVM bindings). So, I want to be able to generate
LLVM IR completely in Common Lisp (using some S-exp syntax for it).
This is what I aim at CG-LLVM project.

But then still, to make use of this code I need to transfer it to LLVM JIT.
For this, I need to be able to dump it to bitcode format
(because there are LLVM C API function that accepts such format).
This is what this project is for: read and write from/to LLVM IR in bitcode format
to LLVM IR S-exp syntax.
