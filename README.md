# HITAGIMON

A bare metal application which runs on my custom i960SB based development hardware. 
The monitor is not actually a monitor but closer to a framework that the application sits
on top of. 

Currently, the program that I am bringing up is DOOM. I have no illusions that the cpu
will be fast enough to run DOOM but it is a very good test of my AVR based chipset. 
It is written in a combination of C and C++98 (gcc 3.4.6 is the last supported version...).
So I don't get C++11 and later but I still get quite a lot of nice features afforded by C++. 

The design of this program is broken up into several components:

1. boot - The code which is used to boot the i960 prior to newlib being functional
   1. This also contains references to the system procedure table and other i960 specific features
2. chipset - The code which is used to interact with the memory mapped io provided by the 1284p based chipset through a very clean C++ interface which is leveraged by sys.
3. sys - Implementations of the system specific routines required by newlibc (sbrk, read, write, etc)
4. cortex - Wrapper classes around C library functionality plus anything else that C++ makes easier
5. program - The DOOM source code + hitagimain

Hitagimon does not use the system procedure table at this point and is closer to an arduino program in its design. This
choice was made due to the fact that there is no operating system nor indirect services that I have to insulate from the
program itself.

## NOTES
This project requires access to the i960-elf-gcc 3.4.6 cross compiler to build.

The reference directory contains the source code and documentation for NINDY which is where this application started
and then forked off in a better direction.
