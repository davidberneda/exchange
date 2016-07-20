# exchange
Experiments with variable and array "exchange" methods

Benchmarking different ways to "swap" or "exchange" variables ( A <--> B ) and array elements ( X[a] <--> X[b] )

Testing Windows 32bit x86 and 64bit cpus compilers

Using Delphi (Berlin 10.1) and FreePascal

Testing variables and arrays of Integer 32bit, Int64 and Double (8 bytes float)

Note:
As stated in FastBASM [http://fastbasm.blogspot.com.es/2010/10/consideration-on-using-xchg-instruction.html], using the assembler XCHG instruction is much slower than MOV in modern multi-core cpu processors, as it does an implicit LOCK
