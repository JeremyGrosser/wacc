# Writing a C Compiler (Ada implementation)

Book: https://nostarch.com/writing-c-compiler  
Author site: https://norasandler.com/book/  
Ada forum thread: https://forum.ada-lang.io/t/writing-a-c-compiler/1024

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

The book links to a paper describing three options for AST data structures with
a preference towards an object oriented implementation allowing use of the
visitor pattern. I'm going to stick with discriminated record types for now,
but will move to objects if needed.

Translating ASDL to discriminated records seems to be working well. If this
pattern continues to go well, I'll likely write an automated asdl2ada
generator.

Codegen went suprisingly smoothly. Having a separate data structure for
assembly seems like overkill at this point, but I can see how it might be
useful for optimization passes later.

## Chapter 2: Unary Operators
This chapter adds the first two character `--` token to the lexer, even though
we're not adding it to the AST yet. Rather than adding a single character
lookahead to the IO package, I allow the `Advance` method to take a negative
`Count` argument. I think this keeps the IO interface simpler.

Once again, translating ASDL to record types was pleasant. After adding the new
node types to the AST, the compiler happily provided errors about missing cases
in the parser and codegen packages. Very useful for figuring out what code
needs to be updated.

Three Address Code is a new concept for me. This is where the book diverges
from the interpreters I've written where everything just gets shoved into a
stack machine. It reminds me of SSA (Static Single Assignment) which I've read
about in the LLVM docs.

I had to delete the existing assembly codegen to make room for this new TACKY
stage. I wonder if this means every new compiler pass will force a refactor of
the subsequent passes or if this is just a particularly disruptive one.

Dealing with three or four different tree representations makes me glad I put
them in separate package namespaces and avoided `use` clauses. If we add a lot
more ASDL representations, I'm definitely going to want to take time to write
an ASDL to Ada compiler. The pretty-printing bits are getting especially
repetitive.

The note about hardware register aliases on page 40 makes me wish that they
hadn't picked the x64 ISA to target. There's just so much awkward legacy stuff
there. Further, this confusion of register sizes reminds me of C's weak type
system. No doubt this will make things *interesting* later on.

The tables on page 41 make it pretty clear how TACKY translates to the Assembly
AST. I'm not sure I could have come up with that mapping on my own, but maybe
it'll make more sense after I've implemented the whole book.

The "fixup" pass of assembly generation is awkward, mainly because I need to
replace one instruction with two and I stored them all in a Vector, which can't
be modified while iterating. I iterate through the vector, creating a list of
edits and their offsets. I then iterate through the list of edits in reverse
order and perform the inserts. It's clunky, but it works. Maybe a linked list
or building a new copy of the Vector would be better.

I haven't been freeing memory anywhere up to this point. I did try to
deallocate a node during one of the assembly passes, but this somehow led to an
`Unbounded_String` getting set to null while still referenced. Maybe there's
some string interning happening that I'm not aware of? In any case, I still
feel that deallocation is unnecessary in the context of a short lived compiler
process. Perhaps I'd feel differently if this code was getting embedded into a
language server. If I do want to do deallocation later, I wonder if there's
something clever I can do with storage pools to free an entire tree.
