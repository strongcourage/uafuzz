#!/usr/bin/env python

import re
import sys
import argparse
import os
import shutil
from shutil import copyfile
import glob

afl_path = os.environ['AFL']
afl_fuzz = afl_path + "/afl-fuzz"
default_in_dir = "in"
seed_input = "./in/in"
log_mode = "result" # debug


def main(bin_file, in_dir, out, qemu, cmd, timeout):
    bin_name = os.path.basename(bin_file)
    suffix = "_aflqemu_" if qemu else "_afl_"
    out_dir = os.path.join(bin_file + suffix + timeout, out)
    
    # create an output dir
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)

    # copy binary to output dir
    tmp_file = out_dir + "/" + bin_name
    copyfile(bin_file, tmp_file)
    os.chmod(tmp_file, 0o777)
    os.chdir(out_dir)

    # create uafuzz input directory
    if not in_dir:
        afl_in_dir = os.path.join(out_dir, default_in_dir)
        if not os.path.exists(afl_in_dir):
            os.makedirs(afl_in_dir)
            # create a seed file
            os.system("echo \"\" > " + seed_input)
    else:
        afl_in_dir = in_dir

    # create uafuzz output directory
    afl_out_dir = os.path.join(out_dir, "out")
    if os.path.exists(afl_out_dir):
        shutil.rmtree(afl_out_dir)
    os.makedirs(afl_out_dir)

    # parse mode
    qemu_cmd = " -Q" if qemu else ""
    afl_cmd = afl_fuzz + qemu_cmd + " -m none -i " + afl_in_dir + " -o " + afl_out_dir + " " + cmd

    # set AFL_PATH env
    os.environ["AFL_PATH"] = afl_path
    os.system(afl_cmd)

    
# Parse the input arguments
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--bin_file', type=str, required=True,
                            help="Full path of binary file")
    parser.add_argument('-i', '--in_dir', type=str, required=False,
                            help="Full path of afl input directory")
    parser.add_argument('-o', '--out_dir', type=str, required=False,
                            help="Full path of afl output directory")
    parser.add_argument('-to', '--timeout', type=str, required=False,
                            help="Timeout in minutes")
    parser.add_argument('-r', '--cmd', type=str, required=True, 
                            help="Running command")
    parser.add_argument('-Q', '--qemu', action='store_true', help="QEMU mode")
    
    args = parser.parse_args()

    if not args.in_dir:
        args.in_dir = ""

    if not args.out_dir:
        args.out_dir = "afl"

    main(args.bin_file, args.in_dir, args.out_dir, args.qemu, args.cmd, args.timeout)