# The Beans Cool Compiler 
## About
Beans is an work in progress low-level programming language designed with simplicity and powerful low level control in mind. 

The majority of the language features are not yet implemented, but it is in a state where it might be fun to play around with.
It currently uses https://c9x.me/compile/ as a code generator, although plans to hand roll a code generator still remain.  If you would like to contribute or play around with it, send me a PM and I am happy to answer any questions.

## Building

Currently, QBE only works as an executable and has no library interface. Because of this, the QBE executable must be built and placed in the directory where the BCC  is used.  I would like to try and get QBE at least statically linked into the BCC compiler with some patching but I need some time to get that setup.  For now, there is a script called ``configure`` that downloads and builds the QBE executable.  In addition to QBE, you will also need GCC to assemble and link the outputted assembly. 

### Steps

``git clone https://github.com/garfr/bcc/``

``cd bcc/``

``./configure``

``make all``

## Language Examples

(wip)
