# HITAGIMON

A bare metal application which runs on my custom i960SB based development hardware. 
I am planning to turn the i960 into an emulator to run i960 on top of i960. Why? 
To implement support for operations from the Protected Architecture. 
Eventually, I want to also use it to run other architectures as well on top of the i960.

The design of this program is broken up into several components:

- boot - The code which is used to boot the i960 prior to newlib being functional
  - This also contains references to the system procedure table and other i960 specific features
- sys - Implementations of the system specific routines required by newlibc (sbrk, read, write, etc)
- cortex - Wrapper classes around C library functionality plus anything else that C++ makes easier, it also contains the routines to interface with IO devices
- reference - NINDY source code reference
- emulation - Code relating to emulating other platforms
