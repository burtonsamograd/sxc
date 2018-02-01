Linear Lisp
-----------

An implementation of a Linear Lisp, as described by Baker:

http://home.pipeline.com/~hbaker1/LinearLisp.html

Abstract

Linear logic has been proposed as one solution to the problem of
garbage collection and providing efficient "update-in-place"
capabilities within a more functional language. Linear logic conserves
accessibility, and hence provides a mechanical metaphor which is more
appropriate for a distributed-memory parallel processor in which
copying is explicit. However, linear logic's lack of sharing may
introduce significant inefficiencies of its own.

We show an efficient implementation of linear logic called Linear Lisp
that runs within a constant factor of non-linear logic. This Linear
Lisp allows RPLACX operations, and manages storage as safely as a
non-linear Lisp, but does not need a garbage collector. Since it
offers assignments but no sharing, it occupies a twilight zone between
functional languages and imperative languages. Our Linear Lisp Machine
offers many of the same capabilities as combinator/graph reduction
machines, but without their copying and garbage collection problems.

WIP

--
Burton Samograd

2018