BrianScheme
===========

This is a non-compliant implementation of the Scheme language.

The C code implements a simple s-expression reader/writer, an
interpreter, garbage collection, foreign function interface, and a
simple virtual machine for an internal bytecode format.

The SCH code implements a base library, a compiler from scheme to the
bytecode format mentioned above, and a JIT assembler that utilizes
libjit (accessed via the FFI system) to allow the creation of new
primitive functions. Also implemented in SCH is the extensible reader,
printer, and repl. The eventual goal is to write a bytecode ->
assembly translator so that we can compile the full language down to
the processor's machine code.

A non-compliant implementation of CLOS is also included and is used as
the foundation for the extensible reader and printer.

BrianScheme also has the ability to save images of its current state
as a file on disk. It can then restart at that state very quickly
making it a suitable language for command line utilities.

You can read more about the implementation at these links:

* [BrianScheme](http://nullprogram.com/blog/2011/01/11/)
* [Bootstrapping and Images](http://nullprogram.com/blog/2011/01/30/)

The primary implementors maintain blogs at:

* [50Ply](http://50ply.com)
* [NullProgram](http://www.nullprogram.com)

LICENSE:

Copyright 2012 Brian Taylor

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

