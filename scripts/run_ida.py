#!/usr/bin/env python

# Usage:
# $UAFUZZ_PATH/binsec/src/ida/run_ida.py -i /path/to/binary --ida_graph --simple

import re
import sys
import argparse
import os
import shutil
from shutil import copyfile
import glob

ida_path = os.environ['IDA_PATH']
graph_easy_path = os.environ['GRAPH_EASY_PATH']
binsec_bin_path = os.environ['UAFUZZ_PATH'] + "/binsec/src/binsec"
ida_script_path = os.environ['UAFUZZ_PATH'] + "/binsec/src/ida/ida.py"
log_mode = "result" # "debug"


# Remove IDA's crashing log files
def fix_ida():
    for file in os.listdir("/tmp/ida/"):
        file_name, file_ext = os.path.splitext(file)
        if file_ext == '.dmp':
            os.remove("/tmp/ida/" + file)


def main(bin_file, ida_graph, simple):
    fix_ida()
    print "Processing binary: " + bin_file
    bin_dir, bin_name = os.path.split(bin_file)
    out_dir = os.path.join(bin_dir, bin_name + "_ida")

    # create an output dir
    if os.path.exists(out_dir):
        shutil.rmtree(out_dir)    
    os.makedirs(out_dir)
    
    # intermediate files
    tmp_file = out_dir + "/" + os.path.basename(bin_file)
    idb_file = tmp_file + ".idb"
    ida_file = tmp_file + ".ida"

    # copy binary to output dir
    copyfile(bin_file, tmp_file)
    os.chmod(tmp_file, 0o777)
    os.chdir(out_dir)

    # run script to get *.idb
    ida_cmd = ida_path + " -B \"-S" + ida_script_path + " -o=" + out_dir + "\" " + tmp_file
    os.system(ida_cmd)
    
    # get *.ida file
    graph_cmd = " -i=True" if ida_graph else ""
    parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + " -cg=True" + \
                    graph_cmd + "\" " + db_file
    os.system(parse_cmd)
    
    # convert call graph into dot format
    cg_cmd = graph_easy_path + " --input=callgraph.gdl --output=callgraph.dot 2> /dev/null"
    os.system(cg_cmd)
    
    # run binida
    simple_cmd = "-ida-no-simple" if simple else ""
    binida_cmd = binsec_bin_path + " -ida -isa x86 -quiet -ida-cfg-dot " + simple_cmd + \
                    " -ida-o-ida " + ida_file + " -ida-loglevel " + log_mode
    os.system(binida_cmd)


# Parse the input arguments
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--bin_file', type=str, required=True, 
                            help="Full path of binary file")
    parser.add_argument('-o', '--ida_graph', action='store_true',
                            help="Original IDA graph (one basic block can contain multiple call instructions)")
    parser.add_argument('-s', '--simple', action='store_true',
                            help="Generate simple graph whose each node is the first instruction of basic block")
    
    args = parser.parse_args()

    main(args.bin_file, args.ida_graph, args.simple)