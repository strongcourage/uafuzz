#!/usr/bin/env python

import re
import sys
import argparse
import os
import shutil
from shutil import copyfile
import glob
from collections import defaultdict,OrderedDict
import networkx as nx
from pprint import pprint

ida_path = os.environ.get('IDA_PATH')
uafuzz_path = os.environ.get('UAFUZZ_PATH')
uafbench_path = os.environ['UAFBENCH_PATH']
ida_script_path = uafuzz_path + "/binsec/src/ida/ida.py"
bin_file = out_dir = ""
funcs = defaultdict(list)
bb_addrs = OrderedDict()
log_mode = "result" # debug


# Remove IDA's crashing log files
def fix_ida():
    if os.path.exists("/tmp/ida/"):
        for file in os.listdir("/tmp/ida/"):
            file_name, file_ext = os.path.splitext(file)
            if file_ext == '.dmp':
                os.remove("/tmp/ida/" + file)


# Run IDA to get ida file
def run_binida():
    fix_ida()
    # create an output dir
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    
    # intermediate files
    tmp_file = os.path.join(out_dir, os.path.basename(bin_file))
    idb_file = tmp_file + ".idb"
    ida_file = tmp_file + ".ida"

    # copy binary to output dir
    if not os.path.isfile(tmp_file):
        copyfile(bin_file, tmp_file)
        os.chmod(tmp_file, 0o777)
    os.chdir(out_dir)  

    # run script to get *.idb
    ida_cmd = ida_path + " -B \"-S" + ida_script_path + " -o=" + out_dir + "\" " + tmp_file
    os.system(ida_cmd)
    print ida_cmd
    
    # generate modified IDA CFG (each block has at most 1 call instruction)
    parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + " -cg=True\" " + idb_file
    os.system(parse_cmd)
    
    # run binida
    binida_cmd = " -ida -isa x86 -quiet -ida-o-ida " + ida_file + " -ida-loglevel " + log_mode

    return ida_file


# Parse ida file of binary's CFGs
def parse_ida(ida_file):
    with open(ida_file) as idaf:
        func_addr = 0
        func_name = ""
        bb_addr = 0
        for line in idaf:
            # parse functions
            if line[0] == "{":
                words = line[1:-1].split(";")
                func_addr = words[0]
                func_name = words[1][:-1]
                funcs[func_name] = defaultdict(list)
            # parse basic blocks
            elif line[0] == "[": 
                words = line[1:-1].split(";")
                bb_addr = words[0]
                insts = words[1][1:-1].split(",")
                for inst in insts:
                    funcs[func_name][bb_addr].append([inst, 0, ""])
            # parse instructions
            elif line[0] == "(":
                words = line[1:-1].split(";")
                for inst in funcs[func_name][bb_addr]:
                    if inst[0] == words[0]: # instruction's address
                        inst[1] = words[3] # basic block's address
                        inst[2] = words[1] # instruction
                        bb_addrs[inst[0]] = (inst[1], inst[2])


# Remove suffix "_f" or "_u" if needed
def remove_suffix(node):
    if node.endswith("_f") or node.endswith("_u"):
        return node[:-2]
    return node


# Parse Valgrind's output to generate target files for fuzzing
def parse_valgrind(valgrind_file):
    valgrind_addrs = []
    events = {}
    possible_targets = defaultdict(list)
    patch_testing = False
    with open(valgrind_file) as vf:
        event_idx = 0
        targets = []
        for line in reversed(vf.readlines()):
            if "Address 0x" in line:
                continue
            end_path = True if "at 0x" in line else False
            words = line.split()

            for i in range(len(words)):
                prev_addr = ""
                if "0x" in words[i]: # interesting addresses
                    addr = words[i][:-1].lower()
                    func_name = words[i + 1]
                    valgrind_addrs.append((addr, func_name))
                    call_addr = hex(int(addr, 16) + 1)
                    bb_addr = "?"
                    # call instruction
                    if call_addr in bb_addrs.keys():
                        idx = bb_addrs.keys().index(call_addr)
                        bb_addr = bb_addrs.values()[idx - 1][0]
                    # bad use
                    elif addr in bb_addrs.keys():
                        idx = bb_addrs.keys().index(addr)
                        bb_addr = bb_addrs.values()[idx - 1][0]
                    # possibly patch testing
                    else:
                        prev_addr = valgrind_addrs[-1][0]
                        prev_func = valgrind_addrs[-1][1]
                        if "0x4" not in prev_addr:
                            patch_testing = True
                            print "[!] Cannot find BB address of (" \
                                    + prev_addr + ": " + prev_func + ")"

                if len(targets) >= 1 and patch_testing:
                    caller_name = targets[-1][1]
                    for bb in funcs[caller_name]:
                        for inst in funcs[caller_name][bb]:
                            callee_name = func_name
                            lib_callee_name = "_" + callee_name # _malloc, _free
                            if "call" in inst[2] and \
                                (callee_name in inst[2] or lib_callee_name in inst[2]):
                                prev_line = valgrind_addrs[-2]
                                poss_target = (inst[1], caller_name)
                                if poss_target not in possible_targets[prev_line]:
                                    possible_targets[prev_line].append(poss_target)
                                targets[-1][0] = inst[1] # assign the last one to bb addr

                if "0x8" in words[i]:
                    targets.append([bb_addr, func_name])

            if end_path is True and len(targets) > 0:
                if event_idx == 0:
                    events['alloc'] = targets
                elif event_idx == 1:
                    events['free'] = targets
                else:
                    events['use'] = targets
                targets = []
                event_idx += 1
            else:
                continue
        
        if len(events) != 3: # special case with only one path
            print "[!] There is not 3 UAF events in Valgrind's report!"
            events['free'] = events['use'] = events['alloc']
        else:
            root = events['alloc'][0]
        print "[+] Alloc path: " + str(events['alloc'])
        print "[+] Free path: " + str(events['free'])
        print "[+] Use path: " + str(events['use'])
        if len(dict(possible_targets)) > 0:
            print "[+] Possible targets:"
            pprint(dict(possible_targets))

    alloc_nodes = [i[0] + "," + i[1] for i in events['alloc']]
    free_nodes = [i[0] + "," + i[1] for i in events['free']]
    use_nodes = [i[0] + "," + i[1] for i in events['use']]

    # identify the root node
    root = ""
    if "main" in alloc_nodes[0]:
        root = alloc_nodes[0]
    elif "main" in free_nodes[0]:
        root = free_nodes[0]
    elif "main" in use_nodes[0]:
        root = use_nodes[0]
    else:
        print "[!] No root node in main"

    # handle case where UAF paths have different roots in main
    if root not in free_nodes:
        free_nodes = [root] + free_nodes
    if root not in use_nodes:
        use_nodes = [root] + use_nodes
    
    # build a Dynamic Calling Tree (DCT)
    G = nx.OrderedDiGraph()
    for i in range(len(alloc_nodes) - 1):
        G.add_edge(alloc_nodes[i], alloc_nodes[i + 1])
    for i in range(len(free_nodes) - 1):
        G.add_edge(free_nodes[i], free_nodes[i + 1])
    for i in range(len(use_nodes) - 1):
        G.add_edge(use_nodes[i], use_nodes[i + 1])
    p = nx.drawing.nx_pydot.to_pydot(G)
    p.write_png(os.path.join(out_dir, "dct.png"))

    # preorder traversal
    traversed_nodes = list(nx.dfs_preorder_nodes(G, source=root))
    # print "[+] Traversed nodes: " + str(traversed_nodes)

    # check whether UAF paths share same nodes
    free_ids = []
    use_ids = []
    for i in range(1, len(free_nodes)):
        if free_nodes[i] in traversed_nodes:
            if traversed_nodes.index(free_nodes[i]) < traversed_nodes.index(free_nodes[i - 1]) and \
                free_nodes[i] in alloc_nodes:
                print "[*] Should modify node " + free_nodes[i]
                for j in range(i, len(free_nodes)):
                    free_ids.append(j)
    for i in range(1, len(use_nodes)):
        if use_nodes[i] in traversed_nodes:
            if traversed_nodes.index(use_nodes[i]) < traversed_nodes.index(use_nodes[i - 1]) and \
                use_nodes[i] in (alloc_nodes + free_nodes):
                print "[*] Should modify node " + use_nodes[i]
                for j in range(i, len(use_nodes)):
                    use_ids.append(j)

    DCT = G
    # build a modified DCT
    if len(free_ids) > 0 or len(use_ids) > 0:
        # print free_ids, use_ids
        G1 = nx.OrderedDiGraph()
        for i in free_ids:
            if free_nodes[i] in alloc_nodes:
                free_nodes[i] = free_nodes[i] + "_f"
        for i in use_ids:
            if use_nodes[i] in (alloc_nodes + free_nodes):
                use_nodes[i] = use_nodes[i] + "_u"

        for i in range(len(alloc_nodes) - 1):
            G1.add_edge(alloc_nodes[i], alloc_nodes[i + 1])
        for i in range(len(free_nodes) - 1):
            G1.add_edge(free_nodes[i], free_nodes[i + 1])
        for i in range(len(use_nodes) - 1):
            G1.add_edge(use_nodes[i], use_nodes[i + 1])

        DCT = G1
        p = nx.drawing.nx_pydot.to_pydot(G1)
        p.write_png(os.path.join(out_dir, "dct_final.png"))

    # UAF bug trace
    bug_trace = list(nx.dfs_preorder_nodes(DCT, source=root))
    for i in range(len(bug_trace)):
        bug_trace[i] = remove_suffix(bug_trace[i]);
    print "[+] UAF bug trace: " + str(bug_trace)

    # distance's targets: all nodes in UAF bug trace
    bin_name = os.path.basename(bin_file)
    fd = open(os.path.join(out_dir, bin_name + ".tgt"), "w+")
    for node in bug_trace:
        fd.write(node + "\n")
    fd.close()

    # cutedge's targets: all edges in DCT
    fc = open(os.path.join(out_dir, bin_name + ".tgt_cut"), "w+")
    for (src, dst) in DCT.edges():
        fc.write(remove_suffix(src) + ";" + remove_suffix(dst) + "\n")
    fc.close()

    # UAF targets: all leaf nodes in DCT
    ft = open(os.path.join(out_dir, bin_name + ".tgt_uaf"), "w+")
    ft.write(remove_suffix(alloc_nodes[-1]) + "\n" + remove_suffix(free_nodes[-1]) + \
                "\n" + remove_suffix(use_nodes[-1]))
    ft.close()


def main(bin_file, valgrind_file, no_ida):
    if no_ida:
        bin_name = os.path.basename(bin_file)
        ida_file = uafbench_path + "/ida/" + bin_name + "/" + bin_name + ".ida"
    else:
        ida_file = run_binida()
    parse_ida(ida_file)
    parse_valgrind(valgrind_file)

    
# Parse the input arguments
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--bin_file', type=str, required=True,
                            help="Full path of binary file")
    parser.add_argument('-v', '--valgrind_file', type=str, required=True,
                            help="Full path of Valgrind's stack traces")
    parser.add_argument('-o', '--out_dir', type=str, required=False,
                            help="Full path of UAFuzz output directory")
    parser.add_argument('--no_ida', default=False, action='store_true')

    args = parser.parse_args()

    bin_file = args.bin_file
    out_dir = args.out_dir

    main(args.bin_file, args.valgrind_file, args.no_ida)