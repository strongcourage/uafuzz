#!/usr/bin/env python
from capstone import *
import trace_pb2
import sys
import struct

md = Cs(CS_ARCH_X86, CS_MODE_32)

index=1

def pp_address_type(a):
    if(a==0):
        return "8 bits"
    elif(a==1):
        return "16 bits"
    elif(a==2):
        return "32 bits"
    elif(a==3):
        return "64 bits"
    elif(a==4):
        return "128 bits"
    else:
        return "???"

def get_address(a):
    if(a.typeid==0):
        return hex(a.value_8)
    if(a.typeid==1):
        return hex(a.value_16)
    if(a.typeid==2):
        return hex(a.value_32)
    if(a.typeid==3):
        return hex(a.value_64)
    if(a.typeid==4):
        return hex(a.value_128)
    else:
        return "???"

def pp_header_archi(a):
    if(a==0):
        return "X86"
    elif(a==1):
        return "X86_64"
    else:
        return "???"

def pp_header(h):
    print "Arch: "+pp_header_archi(h.architecture)+" Size: " + pp_address_type(h.address_size)

def pp_metadata(m):
    return "metadata TODO"

def get_opcode(op):
    tmp=""
    for i in md.disasm(op, 0x0):
        tmp=tmp+i.mnemonic+" "+i.op_str
    return tmp

def pp_read_register(r):
    return "R["+r.name+"]="+get_address(r.value)

def pp_write_register(r):
    return "W["+r.name+"]="+get_address(r.value)

def pp_store_memory(m):
    return "S@["+get_address(m.address)+"]="+get_address(m.value)

def pp_load_memory(m):
    return "L@["+get_address(m.address)+"]="+get_address(m.value)

def pp_call(c):
    return "Call:"+get_address(c.func_addr)+"("+c.func_name+")"+("traced" if c.is_traced else "not traced")

def pp_ret(r):
    return "Ret:"+get_address(r)

def pp_syscall(sys):
    if sys.info.typeid == 0:
        return "SYSCALL:Open"
    elif sys.info.typeid == 1:
        info = sys.info.read_syscall
        addr = get_address(info.buffer_address)
        return "SYSCALL:Read(fd:%i, addr:%s, count:%i, data:%s)" % (info.file_descriptor, addr, info.count, info.buffer_data)
    elif sys.info.typeid == 2:
        return "SYSCALL:Write"

def pp_comment(c):
    return "Comment:"+c

def pp_con_info(ci):
    handler = {0:pp_read_register,1:pp_write_register,2:pp_load_memory,3:pp_store_memory,4:pp_call,5:pp_ret,6:pp_syscall,7:(lambda x: "notretrieved"), 8:pp_comment}
    param = {0:ci.read_register, 1:ci.write_register, 2:ci.load_memory, 3:ci.store_memory, 4:ci.call, 5:ci.write_register, 6:ci.system_call, 7:None, 8:ci.reserved_comment}
    return handler[ci.typeid](param[ci.typeid])
        
def get_con_infos(cis):
    tmp=""
    for ci in cis:
        try:
            tmp=tmp+pp_con_info(ci)+" "
        except UnicodeDecodeError:
            continue
    return tmp

def pp_instruction(i, ins):
    #return pp_address(ins.address)+" "+ins.opcode.decode("hex")+" "+pp_opcode(ins.opcode) +"\n"+pp_con_infos(ins.concrete_info)
    addr = get_address(ins.address)
    opc = get_opcode(ins.opcode)
    concs = get_con_infos(ins.concrete_info)
    print i,addr, "t["+str(ins.thread_id)+"]\t", opc,"\t\t", concs


def pp_body(b):
    global index
    for elem in b:
        if(elem.typeid==0):
            pp_metadata(elem.metadata)
        elif(elem.typeid==1):
            pp_instruction(index,elem.instruction)
            index +=1

def pp_trace(t):
    pp_header(t.header)
    pp_body(t.body)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Please provide a trace in parameter"
        sys.exit(1)

    file_name=sys.argv[1]

    f=open(file_name, "rb")
    size,=struct.unpack("I",f.read(4))
    header = trace_pb2.header_t()
    header.ParseFromString(f.read(size))
    pp_header(header)

    chunk = trace_pb2.chunk_t()
    while True:
        tmp = f.read(4)
        if tmp == "":
            break
        else:
            print "New chunk"
            size, = struct.unpack("I",tmp)
            chunk.ParseFromString(f.read(size))
            pp_body(chunk.body)

    f.close()

