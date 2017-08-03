BPIPE_LIB=modules
export BPIPE_LIB

mkdir -p data
bpipe run -n 1 -d data mdd.groovy $*
