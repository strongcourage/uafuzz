## UAFuzz
The structure of [`src/uafuzz`](./) is as follows:
- `uafuzz_main.ml(i)`: the main function.
- `uafuzz_metrics.ml(i)`: computes seed metrics (e.g., distance metric, cut-edge coverage metric ...).
- `uafuzz_options.ml(i)`: identifies input arguments.
- `uafuzz_params.ml(i)`: identifies fuzzing parameters sent to the fuzzer.
- `uafuzz_targets.ml(i)`: identifies and preprocesses targets.
- `uafuzz_utils.ml(i)`: some utilities.
- `afl-2.52b/afl-showmap.c`: runs the tested binary with an input and displays the execution information, e.g., the trace bitmap (for debugging). 
- `afl-2.52b/afl-fuzz.c`: the core fuzzing of AFL.

### Usage
We interface the fuzzing code in C (see [AFL's README](https://lcamtuf.coredump.cx/afl/README.txt)) with the static part of BINSEC in Ocaml to share information such as target locations, time budget and fuzzing status. Overall we support all principal fuzzing options of AFL as follows.
~~~
$UAFUZZ_PATH/binsec/src/binsec -uafuzz-help
* Help for UAFuzz interface (uafuzz)
  
  -uafuzz               Enable uafuzz
  -uafuzz-I {aflgob|heb|uafuzz}
          Set input metrics [uafuzz]
  -uafuzz-M {fuzz|showmap}
          Set run mode [fuzz]
  -uafuzz-T             Set UAF bug trace [target]
  -uafuzz-d             Skip deterministic stage [false]
  -uafuzz-debug-level   Set uafuzz debug level [0]
  -uafuzz-f             Set output file [%s] [out_file]
  -uafuzz-help          Display options list for uafuzz
  -uafuzz-i             Set input directory [in]
  -uafuzz-loglevel {info|debug|warning|error|fatal|result}
          Display uafuzz log messages only above or equal this level [info]
  -uafuzz-m             Set memory limit [none]
  -uafuzz-no-Q          Set QEMU mode [true]
  -uafuzz-o             Set output directory [out]
  -uafuzz-quiet         Quiet all channels for uafuzz
  -uafuzz-r             Set running command
  -uafuzz-to            Set timeout (minutes) for fuzzing target reports [0]
  -uafuzz-x             Set optional fuzzer dictionary []
~~~
