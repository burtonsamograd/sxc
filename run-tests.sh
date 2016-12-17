#!/bin/bash

for i in $(find tests -name '*.sxc' | sort -n); do
    echo -n $i...
    { ./sxcc.sh -Wall $i &&
	{ [ -f $i.in ] &&
	    { ./a.out < $i.in | cmp $i.out - ; } ||
	    { ./a.out | cmp $i.out - ; }
	} &&
	echo "passed!"
    } || { echo -e "\n*** failed: Compilation results of $i\n\n" && ./sxc.sh $i ; break ; }
done

rm a.out
