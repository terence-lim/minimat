MINIMAT: Matrix Language in OCaml LLVM (compiler for Columbia University COMS4115)

Coded in OCaml, OCamllex and OCamlyacc, this implements a superset of Micro C 
(a highly stripped-down subset of C: ints, bools, and void types, arithmetic, 
if-else, for, and while statements, and user-defined functions) with flexible 
expressions for manipulating matrix and sequence data types, and compiles it into 
LLVM IR.

It needs the OCaml llvm library, which is most easily installed through opam.
It also optionally calls external gnuplot routines for plotting graphs (public 
domain gnuplot_i library written by N.Devillard included in the src subdirectory.

terence.lim@columbia.edu
