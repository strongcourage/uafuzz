#!/bin/bash
PUT="example"
runmode=$1
timeout=$2
targets=$3

# Checkout source code
cd $PUT; export SUBJECT=$PWD;

# Compile source code
rm example example-asan
gcc -g -m32 example.c -o example
gcc -g -m32 -fsanitize=address example.c -o example-asan

# Prepare working directories
cd $SUBJECT; rm -rf obj-$runmode; mkdir obj-$runmode; export FUZZ_DIR=$SUBJECT/obj-$runmode; cd $FUZZ_DIR
mkdir in; echo "" > in/in

# Fuzzing
if [ $runmode = "aflqemu" ]; then
	export AFL_PATH=$HOME/afl
	cp $SUBJECT/example .
	timeout -sHUP ${timeout}m $UAFUZZ_PATH/scripts/run_afl.py -f $FUZZ_DIR/example -Q -i $FUZZ_DIR/in -o run -r "$FUZZ_DIR/example @@" -to $timeout
elif [ $runmode = "aflgo" ]; then
	SECONDS=0
	export AFL_PATH=$HOME/aflgo; export AFLGO=$AFL_PATH
	mkdir temp; export TMP_DIR=$FUZZ_DIR/temp
	export CC=$AFLGO/afl-clang-fast; export CXX=$AFLGO/afl-clang-fast++
	export LDFLAGS=-lpthread
	export ADDITIONAL="-targets=$TMP_DIR/BBtargets.txt -outdir=$TMP_DIR -flto -fuse-ld=gold -Wl,-plugin-opt=save-temps"
	echo $'example.c:20\nexample.c:24\nexample.c:11\nexample.c:26' > $TMP_DIR/BBtargets.txt
	cd $SUBJECT; $CC example.c $ADDITIONAL -g -o example
	cat $TMP_DIR/BBnames.txt | rev | cut -d: -f2- | rev | sort | uniq > $TMP_DIR/BBnames2.txt && mv $TMP_DIR/BBnames2.txt $TMP_DIR/BBnames.txt
	cat $TMP_DIR/BBcalls.txt | sort | uniq > $TMP_DIR/BBcalls2.txt && mv $TMP_DIR/BBcalls2.txt $TMP_DIR/BBcalls.txt
	$AFLGO/scripts/genDistance.sh $SUBJECT $TMP_DIR example
	$CC example.c -distance=$TMP_DIR/distance.cfg.txt -g -o example
	cd $FUZZ_DIR; cp $SUBJECT/example .
	pp_aflgo_time=$SECONDS; echo "pp_aflgo_time: $pp_aflgo_time (s)."
	timeout -sHUP ${timeout}m $AFLGO/afl-fuzz -m none -z exp -c 45m -i in -o out $FUZZ_DIR/example @@
elif [ $1 = "aflgob" ] || [ $1 = "heb" ] || [ $1 = "uafuzz" ]; then
	cp $SUBJECT/example $PUT; cp $targets .
	$UAFUZZ_PATH/scripts/preprocess.py -f $PUT -v $targets -o $FUZZ_DIR
	$UAFUZZ_PATH/scripts/run_uafuzz.py -f $FUZZ_DIR/$PUT -M fuzz -i $FUZZ_DIR/in -o run -r "$FUZZ_DIR/$PUT @@" -I $runmode -T "$FUZZ_DIR/$PUT.tgt" -to $timeout
fi
