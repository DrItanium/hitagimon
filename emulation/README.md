# Emulation

This directory contains all the code relating to performing emulation on top of the i960 itself.

The i960 is a dead platform and porting software to it is very painful due to the fact that you have to use an old version of gcc (3.4.6). 
Attempts have been made to port 11.3.0 but it results in bogus code being generated which is hard to debug and even harder to fix. 
Instead of attempting to cargo cult our way to victory, the next best thing is to perform runtime emulation of other far better supported
platforms. 

I have wanted to do this for a long time and now I have the perfect environment to test all of this out in. 


