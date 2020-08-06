#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
For later:
from collections import namedtuple
MyStruct = namedtuple("MyStruct", "field1 field2 field3")
m = MyStruct("foo", "bar", "baz")
m = MyStruct(field1 = "foo", field2 = "bar", field3 = "baz")
'''

import dba_pb2

import sys  
reload(sys)  
sys.setdefaultencoding('utf8')

def print_dbainstr(instr):
  if instr.typeid == 1:
    print(string_of_lhs(instr.lhs)+" := "+string_of_dbaexpr(instr.expr, True))
  elif instr.typeid == 2:
    print("goto "+string_of_codeaddress(instr.address)+" "+string_of_dbatag(instr.tags))
  elif instr.typeid ==  3:
    print("goto "+string_of_dbaexpr(instr.expr, True)+" "+string_of_dbatag(instr.tags))
  elif instr.typeid == 4:
    print("if ("+string_of_dbacond(instr.cond)+") goto "+string_of_codeaddress(instr.address)+" else "+str(instr.offset))
  elif instr.typeid == 5:
    print("Stop: "+string_of_dbastate(instr.dbastopstate))
  else:
    print("unsupported")

def string_of_lhs(lhs):
  if lhs.typeid == 1:
    return lhs.name
  elif lhs.typeid == 2:
    return lhs.name+"{"+str(lhs.low)+","+str(lhs.high)+"}"
  elif lhs.typeid == 3:
    return "@["+string_of_dbaexpr(lhs.expr, True)+"]"+string_of_endianess(lhs.endian)+to_indice_str(str(lhs.size))
  else:
    assert False

def string_of_dbaexpr(e,toplevel):
  op = "" if toplevel else "("
  oc = "" if toplevel else ")"
  if e.typeid == 1:
    return e.name
  elif e.typeid == 2:
    return "@["+string_of_dbaexpr(e.expr1, True)+"]"+string_of_endianess(e.endian)+to_indice_str(str(e.size))
  elif e.typeid == 3:
    val = to_indice_str(str(e.bitvector.size))
    return str(e.bitvector.bv)+"₍"+val+"₎"
  elif e.typeid == 4:
    if e.expr1.typeid == 1:
      return string_of_dbauop(e.unaryop)+string_of_dbaexpr(e.expr1, False)
    else:
      return op+string_of_dbauop(e.unaryop)+string_of_dbaexpr(e.expr1, False)+oc
  elif e.typeid == 5:
    se1=string_of_dbaexpr(e.expr1, False)
    se2=string_of_dbaexpr(e.expr2, False)
    return op+se1+string_of_dbabop(e.binaryop)+se2+oc
  elif e.typeid == 6:
    if e.expr1.typeid in [1,2]:
      return string_of_dbaexpr(e.expr1, True)+"{"+("" if e.low==e.high else str(e.low)+",")+str(e.high)+"}"
    else:
      return op+string_of_dbaexpr(e.expr1, False)+"{"+("" if e.low==e.high else str(e.low)+",")+str(e.high)+"}"+oc
  elif e.typeid == 7:
    return op+"ext𝒖 "+string_of_dbaexpr(e.expr1, False)+" "+str(e.size)+oc
  elif e.typeid == 8:
    return op+"ext𝒔 "+string_of_dbaexpr(e.expr2, False)+" "+str(e.size)+oc
  elif e.typeid == 9:
    return op+"if "+string_of_dbacond(e.cond)+" "+string_of_dbaexpr(e.expr1, False)+" else "+string_of_dbaexpr(e.expr2, False)+oc
  elif e.typeid == 10:
    print("Alternatives not transfered")
  else:
    assert False

def string_of_dbacond(c):
  if c.typeid == 1:
    if c.expr in [1,4,6]:
      return string_of_dbaexpr(c.expr, True)
    else:
      return string_of_dbaexpr(c.expr, False)
  elif c.typeid == 2:
    return "¬"+string_of_dbacond(c.cond1)
  elif c.typeid == 3:
    return string_of_dbacond(c.cond1)^" && "^string_of_dbacond(c.cond2)
  elif c.typeid == 4:
    return string_of_dbacond(c.cond1)^" || "^string_of_dbacond(c.cond2)
  elif c.typeid == 5:
    return "true"
  elif c.typeid == 6:
    return "false"

def string_of_endianess(end):
  if end.typeid == 1:
    return "𝐿"
  else:
    return "𝐵"

def string_of_dbauop(buop):
  if buop == 1:
    return "-"
  else:
    return "¬"

def string_of_dbabop(bop):
  return ["", " + ", " - ", " *𝒖 ", " *𝒔 ", " pow ", " / ", " /𝒔 ", " mod𝒖 ", " mod𝒔 ", " || ", " && ",
  " ⨁ ", " :: ", " ≪ ", " ≫𝒖 ", " ≫𝒔 ", " lrotate ", " rrotate ", " = ", " ≠ ", " ≤𝒖 ", " <𝒖 ",
  " ≥𝒖 ", " >𝒖 ", " ≤𝒔 ", " <𝒔 ", " ≥𝒔 ", " >𝒔 "][bop]

def string_of_dbatag(tag):
  return ""
  '''
  if tag.typeid == 1:
    return "  #call with return address at "+string_of_dbacodeaddress(tag.address)
  else:
    return "  #return" 
  '''

def string_of_dbacodeaddress(bv):
  return str(bv.bitvector)

def string_of_codeaddress(addr):
  if addr.typeid == 1: #Local
    return "("+str(addr.offset)+")"
  else:  
    return "("+hex(addr.address.bitvector)+","+str(addr.offset)+")"

def to_indice_str(s):
  matrix = ["₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"]
  ret=""
  for c in s:
    val = int(c)
    ret += matrix[val] if val >= 0 and val <= 9 else "X"
  return ret

if __name__ == "__main__":
    data = open("out.dba", "rb").read()
    trace = dba_pb2.dba_list()
    trace.ParseFromString(data)
    for inst in trace.instrs:
      print_dbainstr(inst)
