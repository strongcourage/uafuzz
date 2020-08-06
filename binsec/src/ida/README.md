## BINIDA Plugin
The BINIDA plugin is a part of [BINSEC v0.3](https://github.com/binsec/binsec/releases/tag/binsec-0.3). 
The goal of this plugin is to extract information of the input binary in
x86 using the disassembler IDA Pro, then construct the control flow graphs (CFGs)
that are represented by the data structure of BINSEC. It provides an
additional option to disassemble x86 binaries apart from the existing module
[`src/disasm`](../disasm) implemented in BINSEC. The structure of the BINIDA code [`src/ida`](./) is as follows: 
- `ida.py`: IDAPython script for parsing the input binary, generating the call
  graph and producing an ida file that contains the binary information. 
- `run_ida.py`: Python script for executing the plugin BINIDA.
- `ida.ml(i)`: the main function.
- `ida_options.ml(i)`: identifies input arguments.
- `ida_cg.ml(i)`: parses the call graph.
- `ida_cfg.ml(i)`: parses the ida file to generate CFGs.
- `ida_utils.ml(i)`: some utilities.

The ida files contain the crucial information of the binary like functions, basic blocks and instructions in
the following formats:
```
Function    {start_addr;func_name}
BasicBlock  [start_addr;(instructions);(bb_preds);(bb_succs);(caller_call_addr-callee_start_addr-caller_return_addr)]
Instruction (addr;disasm;opcodes;bb_start_addr;func_name)
```

### Usage
#### 1. Runing IDAPython script `ida.py`
 - `-o/--output-dir`: the absolute path of the output directory.
 - `-cg/--call-graph` [True/False]: generates the call graph.
 - `-i/--ida-graph` [True/False]: if True, generates the original IDA graph, meaning
   one basic block could have multiple call instructions. Otherwise, we consider
   function calls as basic block boundaries to support fuzzing.
 - `--icfg` (*under development*): generates an interprocedural CGF which could be interesting for 
 some further analysis (e.g., static analysis) but is not required for the current project.

~~~bash
$IDA_PATH -B "-S$UAFUZZ_PATH/binsec/src/ida/ida.py --output-dir=/output/path \
    --call-graph=True --ida-graph=True" /path/to/binary
~~~

#### 2. Running `run_ida.py`
 - `--bin_file`: the absolute path of the x86 binary.
 - `--ida_graph`: similar to the option `--ida-graph` of `ida.py`.
 - `--simple`: nodes of CFG are basic blocks. If not, nodes are instructions.

~~~bash
$UAFUZZ_PATH/scripts/run_ida.py --bin_file /path/to/binary --ida_graph --simple
~~~
