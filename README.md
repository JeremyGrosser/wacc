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

## Chapter 3: Binary Operators
The big topic in this chapter was operator precedence. The "Precedence
Climbing" approach makes more sense to me than the Pratt parsers I've worked
with in the past. The discussion in this chapter was quite detailed and I
appreciated the comparison to recursive descent. It's nice to know the "why" in
addition to the "how". The actual implementation ended up being quite a bit
simpler than I expected based on the reading, which was nice.

The add, subtract, and multiply operators are fairly straightforward, except
that they sometimes need an operand pushed to a temporary register, which is
done in a later compilation pass. I really like how adding a new "micropass"
simplifies the codegen.

Division is a little more complicated because one of the operands needs sign
extension. I was extra confused when the `cdq` instruction disassembled as
`cltd`. It's the same thing, but AT&T and Intel assembly syntax disagree on
what this instruction is called. Confusing! Apparently there's a small list of
instruction aliases like this.

Rather than modifying the instruction vector in place like I did in the last
code emission pass, I built a new copy of the vector while reading the old one.
This was much more straightforward than the edit list approach, at the cost of
additional memory utilization. Seems like a worthwhile tradeoff to me.

I got a bit confused with the signedness of stack offsets in my assembly AST
and ended up smashing the stack a few times. I added a
`subtype Stack_Offset range Integer'First .. -4` to make it impossible to
generate code like that again.

The "extra credit" section adds bitwise (AND, OR, XOR, SHL, SHR) operators. I
implemented it, but the tests don't pass and I'm not entirely sure why. I don't
want to waste too much time debugging this right now, so I've committed it to a
branch and will come back to it later.

## Chapter 4: Logical and Relational Operators
Conditional expressions! There are a lot of new symbols to add to the ASDL
structures, I wish I'd added these incrementally rather than trying to do all
of them at once. The compiler complains about every case statement with missing
types so I had to implement all of them before I could get any useful feedback.

My TACKY generation code had assumed that each binary operator would only
produce a single instruction node. In hindsight, this is obviously wrong and
I was forced to do a lot of refactoring in this stage. The short circuit
evaluation of `&&` and `||` operators means that we have to be careful that the
instructions to evaluate the left hand side are generated first so that we can
add a comparison and jump afterward.

I had a bug in assembly emission where `>=` was generating a `je` instruction
rather than `jge`. This was caught by the book's test suite, but difficult to
track down. The source was a simple typo, but I'm left wondering how I might've
caught this one earlier, or found an easier way to pick it out of the generated
assembly code. I miss having the C source inline in the objdump assembly
output. Might be worth a side quest to figure out how to get that debugging
information into the binary.

## Chapter 5: Local Variables
The bug I encountered in Chapter 1 where `return2` was parsed as the `return`
keyword came back to haunt me. I was missing `'0' .. '9'` character range in
identifier names. Chapter 5 tests include a variable named `void2` which was
incorrectly parsed as a `void` token. After fixing the lexer, I re-ran all of
the previous chapters' tests and everything looks fine now.

The rest of the implementation went fairly smoothly once I got through reading
the chapter, which took longer than usual as I kept losing focus. Variable
resolution just doesn't excite me and feels like a bit of a chore.

I refactored most of the `Unbounded_String` operations on `Identifier`s into a
single `WACC.Strings` package just to avoid duplicating the `Make_Identifier`
logic in both the `Semantic_Analysis` and `TACKY` packages. I expect the
`Strings` package to grow more functionality over time.

## Chapter 6: if Statements and Conditional Expressions
I never liked ternary operators in C. The list of examples for precedence rules
and undefined behavior in this chapter really drive home how dangerous they
are. Still, it's part of the langauge, so we must. I liked how the later stages
of the compiler didn't need updating here. We already have conditional jump
instructions, so adding if statements is really just a frontend concern.

I'm a bit surprised that compound statements get their own chapter, but I'm
sure I'll understand soon.

## Chapter 7: Compound Statements
Oh. Scoping rules. That's why. I like the variable renaming and map copy
approach here. Seems simpler than what Crafting Interpreters does with a
nested tree of scopes you have to traverse to resolve.

I did a bit of refactoring in the semantic analysis pass here to more closely
match the function names in the book's pseudocode. Every time I try to be more
clever than the book, I have trouble remembering why I did it a few days later.
There's a lesson there, certainly.

## Chapter 8: Loops
There's a lot of subtle behavior happening in this chapter that took me a while
to wrap my head around. For loops in particular have a lot of moving parts.
I've fallen into the trap of using `null` pointers to represent optional
values. I may go back and try to fix this at some point, but it would be a
large refactor of the ASDL data structures. I'll live with it for now.

The extra credit `goto` statements I added in chapter 6 are broken now. The
`goto_bypass_init_exp` test has a goto jump to a label inside a `for` loop.
This is supposed to skip executing the `for` initializer. From my understanding
of the assembly output, it does this correctly, but I'm not certain. If the
label the `goto` is jumping to is indeed in the wrong place, I think I need to
add a new fixup pass in the assembly stage that moves the label if it precedes
a `For_Init_Node`. I'm not certain this is the right approach though.

## Chapter 9: Functions
This chapter starts with renaming some of the AST nodes. I took this as an
opportunity to do some more significant refactoring and cleanup. I added
forward declarations for all of the AST types, parsing, and semantic analysis
functions. I had been adding the forward declarations only where needed, but
this requires careful ordering of the subprogram bodies and it was starting to
get hard to keep track of. I now understand why forward declarations are
required by the GNAT-mode style checks.

The AST refactoring has left me wishing I had defined more of an interface to
the data structure, rather than exposing the raw type definitions. If I had
done that, I wouldn't have needed to modify every subprogram that manipulates
the AST. Looking at some other mature compiler implementations, I can see that
the AST API is quite well abstracted. I doubt I'll be making such a large
change at this stage, but I'll definitely remember this for my next compiler
project.

Once again, this chapter has me implementing surprising parts of the C
standard. A lot of things, like nested function declarations, feel like they
weren't really intentional language design decisions but were rules reverse
engineered from the behavior of an existing compiler. I wonder if there's any
written history about how C evolved. I'm a bit spoiled by Ada's extensive
documentation, especially around the early design decisions.

My progress has slowed down quite a bit, as I've gotten pulled into other
projects- the new RP2350 is a particularly interesting ball of yarn to play
with.

The weather in Seattle has been exceptional this summer so I've been spending
more time outside too. I expect to have more time for the compiler when it
starts raining in a few months.

_several months later_

After much procrastination, I started looking at Chapter 9 again. I spent a lot
of time trying to figure out why my compiler wasn't calculating stack offsets
in function calls correctly. I didn't make much progress until I just deleted
the FunCall codegen and started over. I carefully followed the algorithm from
the OCaml and Python
[implementations](https://github.com/nlsandler/c-compiler-implementations).

That still didn't work, so I ended up comparing my compiler's assembly output
to the OCaml reference compiler's output line by line. I thought I was clever
defining `type Stack_Offset is range Integer'First .. -4;` and subtracting
offsets. This only led to confusion, so I went back to using a `Natural`
subtype. After many test/compile/compare iterations, I got my assembly close
enough to the reference and passed the tests, but comparing assembly listings
is not a fun way to debug. I would like to avoid doing that again.
