---
title: Tools
---

BINSEC is the name of the tool platform produced by the BINSEC project.
It is released under the terms of
[LGPL v2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html).

# About

The general objective of the open-source BINSEC platform is to foster
the next generation of binary-level analysis tools, by proposing
binary-level semantic approaches, i.e. analysis methods based on the
meaning of the program, and able to cover all of its behaviors -- or at
least a very significant part.

Our general philosophy consists in building on the success of source-level
formal methods for safety analysis of critical systems, and
adapting these results to binary-level security analysis. Besides, we follow a
pragmatic way, exploring combinations of techniques and trade-offs,
and we base our approach on a bedrock of  basic analysis, complemented by
dedicated domain-specific tools.

BINSEC offers the following features:

* a formal Intermediate Representation (IR)  well adapted to formal analysis;
* basic syntactic disassembly techniques : recursive, linear and combination
  of both;
* partial exploration of the behaviors of a given executable file  (*binary-level symbolic execution*);
* static analysis of all behaviors of a given executable file, in order to
  recover the high-level structure of a (non-obfuscated) binary code.

Currently, BINSEC only supports x86-32/ELF, with an experimental PE loader.

# Contact

Feedback is welcome at `binsec _at_ saxifrage.saclay.cea.fr`

# Downloads

* **20160831**  [Beta2](distrib/binsec-beta2-20160831.tgz) ([md5](distrib/binsec-beta2-20160831.md5))

     This release fixes the tarball issue of the initial beta. It also provides
     minor fixes to the x86 decoder and refactoring to the disassembly module.


* **20160704**  [Beta](distrib/binsec-beta-20160704.tgz) ([md5](distrib/binsec-beta-20160704.md5))

    This is our first public release. We are anticipating some rough edges, though, and
    are welcoming [feedback](mailto:binsec@saxifrage.saclay.cea.fr).


# Documentation

  The code is documented [here](apiref/index.html).

  Tutorials are under way ...
