#!/usr/bin/env python

import re
import sys
import argparse
import os
import shutil
from shutil import copyfile
import glob
import time

ida_path = os.environ['IDA_PATH']
uafuzz_path = os.environ['UAFUZZ_PATH']
graph_easy_path = os.environ['GRAPH_EASY_PATH']
uafbench_path = os.environ['UAFBENCH_PATH']
ida_script_path = uafuzz_path + "/binsec/src/ida/ida.py"
binsec_bin_path = uafuzz_path + "/binsec/src/binsec"
afl_uafuzz_path = uafuzz_path + "/binsec/src/uafuzz/afl-2.52b"
default_in_dir = "in"
seed_input = "in"
log_mode = "result" # debug
no_ida = False


# Remove IDA's crashing log files
def fix_ida():
    if os.path.exists("/tmp/ida/"):
        for file in os.listdir("/tmp/ida/"):
            file_name, file_ext = os.path.splitext(file)
            if file_ext == '.dmp':
                os.remove("/tmp/ida/" + file)


def run_binida(bin_file, out_dir, no_ida):
    fix_ida()
    # create an output dir
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    
    # intermediate files
    bin_name = os.path.basename(bin_file)
    tmp_file = out_dir + "/" + bin_name
    idb_file = tmp_file + ".idb"
    ida_file = tmp_file + ".ida"
    orig_ida_file = tmp_file + ".ida_orig"
    callgraph_file = out_dir + "/callgraph.dot"
    exist_ida_file = uafbench_path + "/ida/" + bin_name + "/" + bin_name + ".ida"
    exist_callgraph = uafbench_path + "/ida/" + bin_name + "/callgraph.dot"

    # copy binary to output dir
    copyfile(bin_file, tmp_file)
    os.chmod(tmp_file, 0o777)
    os.chdir(out_dir)

    if no_ida:
        # copy intermediate files
        os.system("cp " + exist_ida_file + " " + ida_file)
        os.system("cp " + exist_ida_file + "_orig " + orig_ida_file)
        os.system("cp " + exist_callgraph + " " + callgraph_file)
        binida_cmd = " -ida -isa x86 -quiet -ida-o-ida " + ida_file + " -ida-loglevel " + log_mode
    else:
        # run script to get *.idb
        ida_cmd = ida_path + " -B \"-S" + ida_script_path + " -o=" + out_dir + "\" " + tmp_file
        os.system(ida_cmd)

        # generate the original IDA CFG
        parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + \
                        " -cg=True -i=True\" " + idb_file
        os.system(parse_cmd)
        os.system("mv " + ida_file + " " + orig_ida_file)

        # generate modified IDA CFG (each block has at most 1 call instruction)
        parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + " -cg=True\" " + idb_file
        os.system(parse_cmd)

        # convert call graph into dot format
        cg_cmd = graph_easy_path + " --input=callgraph.gdl --output=callgraph.dot 2> /dev/null"
        os.system(cg_cmd)

        # run binida
        binida_cmd = " -ida -isa x86 -quiet -ida-cfg-dot -ida-o-ida " + ida_file + " -ida-loglevel " + log_mode
        
    return binida_cmd


def main(bin_file, in_dir, out, mode, cmd, targets, fuzzer, timeout, no_ida):
    # parse mode
    if mode == "fuzz":
        out_dir = os.path.join(bin_file + "_" + fuzzer + "_" + timeout, out)
        uafuzz_out_dir = out_dir
        if fuzzer in ["aflgob", "heb", "uafuzz"]:
            uafuzz_out_dir = os.path.join(out_dir, "out")
        # create output directory
        if os.path.exists(uafuzz_out_dir):
            shutil.rmtree(uafuzz_out_dir)
        os.makedirs(uafuzz_out_dir)
        # create input directory
        if not in_dir:
            uafuzz_in_dir = os.path.join(out_dir, default_in_dir)
            if not os.path.exists(uafuzz_in_dir):
                os.makedirs(uafuzz_in_dir)
                # create an empty seed file
                os.system("echo \"\" > " + os.path.join(uafuzz_in_dir, seed_input))
        else:
            uafuzz_in_dir = in_dir
        mode_cmd = " -uafuzz-M fuzz -uafuzz-i " + uafuzz_in_dir
    elif mode == "showmap":
        mode_cmd = " -uafuzz-M showmap"
        out_dir = os.path.join(bin_file + "_" + mode + "_" + fuzzer, out)
        uafuzz_out_dir = out_dir

    # run binida
    binida_cmd = run_binida(bin_file, out_dir, no_ida)

    # run UAFuzz
    targets_cmd = " -uafuzz-T " + targets
    uafuzz_cmd = binsec_bin_path + binida_cmd + " -uafuzz -uafuzz-loglevel " + log_mode + mode_cmd + \
                    " -uafuzz-o " + uafuzz_out_dir + " -uafuzz-I " + fuzzer + " -uafuzz-r " + "\"" + \
                    cmd + "\"" + " -uafuzz-to " + timeout + targets_cmd
    print uafuzz_cmd
    
    # set AFL_PATH env
    os.environ["AFL_PATH"] = afl_uafuzz_path
    os.system(uafuzz_cmd)

    
# Parse the input arguments
if __name__ == '__main__':
    start = time.time()
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--bin_file',type=str,required=True,
                            help="Full path of binary file")
    parser.add_argument('-i', '--in_dir', type=str, required=False,
                            help="Full path of UAFuzz input directory")
    parser.add_argument('-o', '--out_dir', type=str, required=True,
                            help="Full path of UAFuzz output directory")
    parser.add_argument('-M', '--mode', type=str, required=True,
                            help="Fuzzing mode (fuzz or showmap)")
    parser.add_argument('-r', '--cmd', type=str, required=True, help="Running command")
    parser.add_argument('-T', '--targets', type=str, required=True, help="Fuzzing targets")
    parser.add_argument('-I', '--fuzzer', type=str, required=True, help="Fuzzer to run")
    parser.add_argument('-to', '--timeout', type=str, required=False, help="Timeout (m)")
    parser.add_argument('--no_ida', default=False, action='store_true')

    args = parser.parse_args()

    if not args.in_dir:
        args.in_dir = ""
    if not args.timeout:
        args.timeout = "0"
    
    main(args.bin_file, args.in_dir, args.out_dir, args.mode, args.cmd, args.targets, args.fuzzer, args.timeout, args.no_ida)
    
    end = time.time()
    print "Preprocessing time: " + str(end - start) + " (s)"