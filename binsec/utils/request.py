#!/usr/bin/env python
from message_pb2 import *
from trace_pb2 import trace_t

import time
import socket
import sys
import struct

if __name__ == "__main__":
  mess = message()
  mess.typeid = message.REQUEST
  mess.request.typeid = request.REQ_NEW_TARGETS
  mess.request.request_targets.start_offset = int(sys.argv[3])
  mess.request.request_targets.target = 0x77FFBBCC if len(sys.argv)<=4 else int(sys.argv[4])
  mess.request.request_targets.limit = int(sys.argv[2])
  tr = open(sys.argv[1]).read()
  mess.request.request_targets.trace.ParseFromString(tr)
  

  data = mess.SerializeToString()

  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect(("localhost", 4000))

  #trace = trace_t()
  #trace.ParseFromString(tr)
  #data = trace.SerializeToString()

  s.send(struct.pack("I",len(data)))
  s.send(data)

  len_data = struct.unpack('!i', s.recv(4))[0]
  data = s.recv(len_data)
  
  mess2 = message()
  mess2.ParseFromString(data)
  print mess2  
  validjmp = [0x403000,0x403004,0x403008,0x40300c,0x403010]
  for target in mess2.answer.target_mess.indirect_targets:
      if target in validjmp:
          print hex(target),"Ok"
      else:
          print hex(target),"Ko"
  
  s.close()
