# Writing a C Compiler (Ada implementation)

Book: https://nostarch.com/writing-c-compiler
Author site: https://norasandler.com/book/

## Chapter 1: A Minimal Compiler

The book recommends using an implementation language with "pattern matching"
features. I think the "prototyped" status of [this RFC](https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-pattern-matching.rst)
means GNAT supports that. I'm also pretty sure this is just syntactic sugar for
a bunch of successive `elsif` blocks, so I think I can manage even if it isn't
well supported.

The "compiler driver" program is contained within [main.adb](src/main.adb).
Calls to `gcc` for preprocessing, assembly, and linking use the `AAA.Processes`
library. The book does assembly and linking in a single step, but I've always
considered those to be discrete operations. I've broken them into separate
steps to make it easier to do multiple object linking later.

I wrote a [WACC.IO](src/wacc-io.ads) package to abstract all of the file
operations. The whole input file is read into memory with `System.Mmap` upon
open and writing uses `Ada.Text_IO`. There are many opportunities for future
optimization.

I pulled a handful of Restrictions and the Jorvik profile into a project-wide
[gnat.adc](gnat.adc) to keep myself from doing anything too crazy.

I had intended to write the package specs `with SPARK_Mode => On`, but that
severely limits the standard library packages available and I kept getting
sidetracked finding or writing alternatives. Dropping the SPARK requirement for
now.

I wrote the lexer by hand rather than using regex as suggested in the book.
`GNAT.Regexp` is not PCRE-compatible and I didn't want to try to work around
the edge cases there. Maybe this will come back to bite me, but seems okay for
now.

My lexer initially caught the "misspelled_keyword" test having `returns`
instead of `return`, which is supposed to pass at this stage according to the
test suite. Ignoring the `\b` part of the regex pattern for keywords and
identifiers fixes it, but doesn't catch this error. The book expects you to
catch this during the parse stage apparently.

I chose to store token literals in `Unbounded_String`. This means every token
allocates a little bit of memory. I don't bother allocating the literal for
single character tokens as it should be obvious. Storing offsets within the
input text would be more efficient and enable contextual error messages later
but I want to avoid tight coupling between the I/O routines and lexer for now.
