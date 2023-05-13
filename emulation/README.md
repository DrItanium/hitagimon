# Emulation

This directory contains all the code relating to performing emulation on top of the i960 itself.

The i960 is a dead platform and porting software to it is very painful due to the fact that you have to use an old version of gcc (3.4.6). 
Attempts have been made to port 11.3.0 but it results in bogus code being generated which is hard to debug and even harder to fix. 
Instead of attempting to cargo cult our way to victory, the next best thing is to perform runtime emulation of other far better supported
platforms. 

I have wanted to do this for a long time and now I have the perfect environment to test all of this out in. 


Not only are cores defined but also the software MMU used to provide a safe environment for testing purposes.
I am taking influence from the OPAL SASOS to construct the virtualized memory environment. 

OPAL divides a single virtual memory address into _protection domains_.
Each domain is made up of one or more _segments_. These segments are contiguous blocks of memory which are one page or
larger in size. Segments can be shared between domains to support shared memory and other features in a very fast alternative to shared memory.
A _thread_ is the unit of execution in OPAL which operates on a protection domain. 

I am taking the concept and making it the basis for the emulation system itself.

Since the Sx,Kx,Jx,Hx,and Cx processors do not contain an MMU it is necessary to emulate one in software. 
Each emulated core is a thread of execution which operates on a single domain at a time. Segments can be 
shared between domains to support shared memory and even emulate bank switching. Conceptually, bank switching becomes the switching of domains with different segments mapped in.

