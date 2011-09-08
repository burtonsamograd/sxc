for i in $(find tests -name '*.sxc' | sort -n); do
    echo -n $i...
    { ./sxc.sh $i > x.c &&
	gcc x.c &&
	{ [ -f $i.in ] &&
	    { ./a.out < $i.in | cmp $i.out - ; } ||
	    { ./a.out | cmp $i.out - ; }
	} &&
	echo "passed!"
    } || { echo -e "\n*** Compilation results of $i\n\n" && ./sxc.sh $i ; break ; }
done