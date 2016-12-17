
sxc=${SXC-./sxc.sh}
delete_temp_file=true
verbose=false

tempfile () {
    name=$1/$2-${RANDOM}$3;
    mkdir -p $(dirname $name)
    touch $name
    echo $name
}

verbose () {
    if [[ $verbose == true ]]; then
	echo $*
    fi
}
	
print_help() {
    echo "sxcc.sh: compile and pass files to CC"
    echo "Options:"
    echo
    echo "--keep-temp-file: do not delete temporary C files after compilation with sxc"
    echo "--verbose: print some internal information about sxcc.sh"
    echo "--help: this message"
}

args=""
sxc_files=""
num_files=0
# iterate over args, handling sxcc options, keeping a list of .sxc files and keeping a list of other args
for i in $*; do
    case $i in
	--keep-temp-file)
	    delete_temp_file=false;;
	--help)
	    print_help;;
	--verbose)
	    verbose=true;;
	*.sxc)
	    sxc_files="$sxc_files $i"
	    : $((num_files++));;
	*)
	    args="$args $i";;
    esac
done

# check for no input files and return error if so
if [[ -z $sxc_files ]]; then
    echo "sxcc: no input files" 1>&2
    exit 1
fi

# generate c file names from sxc file names
c_files=""
for file in $sxc_files; do
    c_file=$(tempfile /tmp $file .c)
    c_files="$c_files $c_file"
done

verbose "args: $args"
verbose "sxc_files: $sxc_files"
verbose "c_files: $c_files"

# convert the files variables to arrays
sxc_files=( $sxc_files )
c_files=( $c_files )

# compile sxc files to c files
for ((i=0; i<$num_files; i++)); do
    $sxc ${sxc_files[i]} > ${c_files[i]}
done

cc_cmd="${CC-gcc} $args ${c_files[*]}"
verbose "cc_cmd: $cc_cmd"
$cc_cmd

# delete temp files unless told not to
if [[ $delete_temp_files == true ]]; then
    rm -f ${c_files[*]}
fi




    
