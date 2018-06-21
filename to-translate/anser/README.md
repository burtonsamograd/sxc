anser - a boolean propagator network framework
----------------------------------------------

"Operator Overloading for Fun and Profit - creating computation networks from ordinary mathematical expressions."

Boolean Propagator Networks allow for the bi-directional computation
of mathematiacal functions.  This means that you can define an
equation and compute any of it's variables using its other variables
as input.  An example is a Farenheit to Celcius convertor encoding the
equation 'farenheight = celcius * 9/5 + 32':

```c
Bus farenheight, celcius;
    
Bus constant, a, b;
b = a * constant;
a.set(5);
b.set(9);
    
farenheight = celcius * constant + 32;
```

This example has 2 'variables' or terminals to the compuatation
network: farenheight and celcius.  There is also an internal variable
to compute the constant 9/5.

Once we've setup the computation network equation, you can evaulate
the equation by doing the following:

```c
celcius.set(0);
assert(farenheight.get() == 32);
```

But, unlike a normal programming statement, we can also compute it in
reverse:

```c
farenheight.set(32);
assert(celcius.get() == 0);
```

This can be done in arbitrary ways, but there are limitations.  This
paper is written to explore and analyze these limitation of binary
propagator networks.

--

'Anser' is a small framework for the creation of boolean logic
computation networks from C++ expressions.  Such networks are
collections of 'wires' (terminals) that connect 'ops' (gates) to
specify boolean functions.

The created networks are non-directional; all computations *will* be
computed and output to the relvant terminal of the computation network
given sufficient information on the remaining termnals.

Information can be entered 'bit by bit' into the terminals, and the
computation graph can then be analyzed for changes in the network.  By
analyzing the changes in the nodes in the graph, it was thought that
one could use this 'emergent' information in the system to glean find
interesting information within the function that is being analyzed.

This libary could also be used to do digital logic design at the
And/Or/Not gate level.

NOTE
----

This library is very leaky memory wise and should be used with a
Garbage Collector like the Bohem GC.

Motivation
----------

By building compuation graphs, one can see the actual logical inner
workings of compuational functions.  This in itself is fascinating.

The main intent of this tool was to create a system to help with the
cracking of hash functions, or at least to learn more about hash
functions along the way.  By analyzing the inner workings and lattice
logic structures of existing hash functions and their informational
flow analysis as bits are entered, we could find flaws or
optimizations and help build future generations of systems.

I want to make a suite of logic design tools that will allow me to
generate the plans for a CPU or some other piece of logic hardware.

Example
-------

```c
#include "anser.h"

int main(int argc, char** argv) {
    // Declare the I/O terminals for our network
    Bus a, b, c;

    // Define/build the computation network
    c = a + b;

    // Evaluate the network by setting terminal values.
    // Compute b, by setting a and c.
    c.set(3);
    a.set(1);

    // Make sure we're correct.
    assert(b.get() == 2);
}
```

Examples
--------

The following contain more complex examples of the system:

tests.cpp	: basic tests for computational networks, including the djb2 hash function

sha256.cpp	: a (mostly) unmodified sha256 implemementation and test;
 		  changes were made to variable types to allow for the use of anser.

Graph Traversal
---------------

Wires offer the functions 'size' and 'nth', allowing for one to
retrieve the connetion Ops of the wire.

UnOps have get_in() and get_out() to retrieve thier terminal Wires.

BinOps have get_in1(), get_in2() and get_out() to retrieve thier
terminal Wires.

Ops follow the following convention to retrieve their terminals:

```c
virtual int inputs()		=> return the number of inputs
virtual Wire *input(int n)	=> get input number n
virtual int outputs()		=> return the number of outputs
virtual Wire *output(int n)	=> get input output n
```

Classes
-------

Wire		- a communication channel for connecting inputs and output termninal on a *Op

Bus	   	- a bundle of 'Wires', 32 by default to be used as integer expression variables
                  utilizing the overloaded integer operations to build the computation networks

--

UnOp		- Unary Operation, 1 terminal operation

BinOp 		- Binary Operation, 2 terminal operation

Op 		- Operation, n terminal operation

--

Id : UnOp	- Identity, input passes through to output

Not : UnOp 	- Logical Not, output = !input

And : BinOp	- Logical And, output = input1 & input2

Or: BinOp	- Logical Or, output = input1 | input2

Xor: BinOp	- Logical Xor, output = input1 ^ input2

HalfAdder: Op	- Binary Half Adder, half adder with a, b, outputs sum and carry

FullAdder: Op   - Binary Full Adder, full adder with a, b, carry, outputs sum and carry

The Use of The Bus
------------------

Busses are abstractions over top of the above lower level logic
machinery used to create computation graphs from ordinary mathematical
expressions.  A bus can be thought of as a variable in your integer
equation, but it is bi-directional...generally.  Here's an example:

```c
    Bus a, b, c;	// define busses

    c = a + b;		// build graph

    a = 1, c = 4;	// submit partial information

    assert(b == 3);	// check result is correct
```

As you can see from the above example, we defined the equation one way
like one would normally compute it in a programming language, but
could solve for values on the right hand side of the equals, unlike a
normal programming expression.

Busses define the following operations:

    + - add, add
    * - mul, multiply
    & - and, logical and
    | - or, logical or
    ^ - xor, logical xor
    >> - shr, shift right
    << - shl, shift left
    >>= - ror, rotate right

This works with arbitrary fixed size integer expressions and data.
The penultimate example so far is SHA256, included in the examples.
There is also an unpublished experiement with performing the bitcoin
double SHA256 hash function and mining operation and computing it
correctly.

Graph Analysis
--------------

The intention of this project is to create a tool for building and
debugging logic networks with the hopes of architecting a computation
system.  As this tool is meant to create computation graphs, as
complete set of graph analysis tools and utilities are going to be
needed.

But.

They might not be found here. This project is a derivative of a 'lisp'
project I have been interested in over the years (see references
below).  The design tools for the computer architecture I am planning
are going to be written in Common Lisp; this project is going to be
the 'execution engine' for the simuation.  Really, this project is an
implementation of the Scheme projects found SICP chapter 3.

It runs faster since it is written in C++.

The real design intention of this project is to be the core runtime of
a graph analysis and generation toolkit written in Common Lisp, with
the goal of such a toolkit to be the creation and analysis of digital
logic machines, such as computers.

Hash Function Analysis
----------------------

One can analyze the internals of functions using anser.  Let's start
with computing a standard hash function using a compuation graph:

```c
#include "anser.h"

// THe original DJB2 hash function
unsigned long
djb2_orig(unsigned char *str)
{
  unsigned long hash = 5381;
  int c;
  
  while ((c = *str++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  
  return hash;
}

// Build a hash function that computes DJB2 using a computation graph
Bus*
djb2_anser(Bus input[16])
{
  Bus* hashn = new Bus, *hash;
  hashn->set(5381);
  hash = hashn;
  for(int i = 0; i < 16; i++) {
    *hashn = ((*hash << 5) + *hash) + input[i]; /* hash * 33 + c */
    hash = hashn;
  }
  
  return hashn;
}

int main() {
    unsigned char* string = (unsigned char*)"0123456789012345";

    Bus input[16];			// define an input bus
    Bus* output = djb2_anser2(input);	// build the computation graph

    // At this point we have the computation graph and can perform any
    // type of analysis on it through it's input and output terminals.
    // This is work in progress.


    // For now we can compute the hash function through the computation graph
    // by individually setting the inputs that we gave earlier.
    for(int i = 0; i < 16; i++) {
      input[i].set(string[i]);
    }

    // Now we can read the output terminal to read the hash value.

    unsigned int hash = djb2_orig(string); // compute hash using the original function

    std::cerr << "------\n";
    std::cerr << std::hex << hash << std::endl;
    std::cerr << std::hex << output->get() << std::endl;
}
```

An analysis of why one can't set the output terminal and get a result
through the propagator network running in reverse is left as an
exercise to the reader.

Division using Propagators
--------------------------

Only multiply is implemented for Busses, but you can perform a
division operation using the magic of propagators.  To perform a
multiplication, you work like so:

```c
Bus a, b, c;

c = a * b;

a.set(2); // a is an input
b.set(4); // b is an input

assert(c.get() == 8); // c is an output
```

To peform division, change your input/output terminals around:

```c
Bus a, b, c;

c = a * b;

a.set(2);  // a is an input
c.set(8)   // c is an input

assert(b.get() == 4); // b is now an output
```

Square Root Using Propagators
-----------------------------

The curious might try the above technique with something more complex,
like a square root function, which would be the reverse of a
single variable multiplication:

    // Square root
    Bus x, y;

    y = x * x;

    y.set(4);
    assert(x.get() == 2);

Running this gives:

    >>> libc++abi.dylib: terminating with uncaught exception of type WireNotSetException*

Unfortunately, this does not seem work, and it is unknown why to the
author.

TODO: Further analysis is required.

An analysis of the Square Root network
--------------------------------------

TODO: learn to traverse graph network.

'dot' graphs
------------

An intention is to display computation graphs using the tool 'dot' from 'graphviz'.

References
----------

The Art of the Propagator (TAOTP) by Alexy Radul and Gerald Jay Sussman:

http://web.mit.edu/~axch/www/art.pdf

Structure and Interpretation of Computer Programs (SICP):

- A Simulator for Digital Circuits: https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.4

- Propagation of Constraints: https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5

--
Burton Samograd
burton.samograd@gmail.com
2016
