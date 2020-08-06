#!/bin/bash

BINSEC_PATH="$UAFUZZ_PATH/binsec"
QEMU_UAFUZZ_PATH="$BINSEC_PATH/src/uafuzz/afl-2.52b/qemu_mode_uafuzz"
QEMU_AFLGOB_PATH="$BINSEC_PATH/src/uafuzz/afl-2.52b/qemu_mode_aflgob"

if [ -z "$UAFUZZ_PATH" ]; then
	printf "Please assign a value to UAFUZZ_PATH !\n"
	exit 1
fi

if [ $# -lt 1 ]; then 
	printf "Usage: build.sh [clean/binsec/aflgob/uafuzz] !\n"
	exit 1
fi

function clean_all() {
	cd "$BINSEC_PATH/src"; OCAMLBUILD=no make clean
	cd "$QEMU_UAFUZZ_PATH/qemu-2.10.0"; make clean
}

function build_binsec() {
	cd $BINSEC_PATH
	autoconf
	./configure
	cd src; OCAMLBUILD=no make clean depend binsec
}

eval `opam config env`

if [ $1 = "clean" ]; then
	clean_all
elif [ $1 = "binsec" ]; then
	build_binsec
elif [ $1 = "aflgob" ]; then
	clean_all
	cd $QEMU_AFLGOB_PATH;
	CPU_TARGET=i386 ./build_qemu_support.sh no; sleep 1s;
	build_binsec
elif [ $1 = "uafuzz" ]; then
	clean_all
	cd $QEMU_UAFUZZ_PATH;
	CPU_TARGET=i386 ./build_qemu_support.sh no; sleep 1s;
	build_binsec
fi
