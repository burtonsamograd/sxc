NOTE: sxc is WIP (Work In Progress)

TODO: make macros work

This project was written over 5 years ago by my past self, and was
found weeks ago by my current self, and this is a note, telling my
future self that I should verify my work prior to publishing.

When I found this project, after looking through the sources and
remembering how far I'd gotten with the project, assumed that all the
code I had written did actually work.

I was wrong.

Unfortunately, the main example, sswitch.sxc was but a dream.  Macros
have not yet been implememted in sxc as of the end of 2016.

I apologize for the inconvenience should you have expected more from
this project at its release.

So did I.

Cursory examination of the problem (adding Common Lisp macros to sxc)
proved to show that this is not a simple problem to handle in Lisps.
I call it the 'printed representation' problem.  It comes not from
Lisp itself, but from the combination of Lisps that are currently
available for use.

How does one describe this problem?

I found it first in something I created using Emacs Lisp.  It was an
elegant solution to the programming problem of web development,
elegantly combined together into a nested sea of S-Expressions
combining a number of code generation languages.  One of those
languages was a compiler that was run as an external program.

There was a macro written in emacs lisp that took the printed
representation of it's argument S-Expressions and passed them to the
external compiler.  This is where the problem reared it's head, but in
subtly strange ways.  Days later, I came to the realization what the
actual problem was.

Lisp has many times.  Read time, compile time and run time.  It has
little syntax, for convenience, like ''' and '`' and ','.  At read
time, the syntax is transformed away and replaced by internal
identifiers, like so:

```lisp
    'x => (quote x)
    `(+ ,x ,@y) => (quasiquote (+ (unquote x) (unquote-splice x)))
```

By the time the S-Expression has been READ by the reader, the syntax
has mostly been stripped. The problem...

We need to jack the reader.

I'll explain more later.

--
Dec 25, 2017