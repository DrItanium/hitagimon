//
// Created by jwscoggins on 5/2/21.
//
#include "IODevice.h"
int wait(int count) {
    int result = 0;
    for (int i = 0; i < count; ++i) {
        result += i;
    }
    return result;
}
int main() {
    BuiltinLED theLed(0);
    BuiltinConsole theConsole;
    BuiltinTFTDisplay theDisplay;
    volatile uint64_t counter = 0;
    std::string msg0("donuts!");
    theConsole.write("hello, world\n");
    theConsole.writeLine(msg0);
    theConsole.write('d');
    theConsole.write('o');
    theConsole.write('n');
    theConsole.write('u');
    theConsole.write('t');
    theConsole.write('s');
    theConsole.write('\n');
    theDisplay.clearScreen();
    uint16_t* colorTable = new uint16_t[0x1000000]();
    for (int i = 0; i < 0x1000000; ++i) {
        colorTable[i] = theDisplay.color565(i);
    }
    uint16_t previousColor = colorTable[0];
    for (int i = 0; i < 0x1000000; ++i) {
        uint16_t color = colorTable[i];
        if (previousColor != color) {
            theDisplay.fillScreen(color);
            counter += wait(1000);
        }
        previousColor = color;
    }
    for (int r = 0; r < 256; ++r) {
        for (int g = 0; g < 256; ++g) {
            for (int b = 0; b < 256; ++b) {
                uint16_t color = theDisplay.color565(r,g, b);
                for (int x = 0; x < 16; ++x) {
                    for (int y = 0; y < 16; ++y) {
                        theDisplay.drawPixel(x,y, color);
                    }
                }
            }
        }
    }
    while(true) {
        theLed.toggle();
        counter += wait(1000000);
        theLed.toggle();
        counter += wait(1000000);
    }
    delete [] colorTable;
    return 0;
}

extern "C"
int atexit(void (*function)(void))
{
     function();
}
#if 0
// functions for back end testing
extern "C"
ssize_t write(int fildes, const void* buf, size_t nbyte) {
    const char* theBuf = (const char*)buf;
    volatile uint16_t& con = getConsoleReadWritePort();
    for (size_t i = 0; i < nbyte; ++i) {
        con = (uint16_t)theBuf[i];
    }
    return nbyte;
}

extern "C"
ssize_t read(int fildes, void* buf, size_t nbyte) {
    char* theBuf = (char*)buf;
    volatile uint16_t& con = getConsoleReadWritePort();
    for (int i = 0; i < nbyte; ++i) {
        theBuf[i] = (char)con;
    }
    return nbyte;
}
extern "C"
int open(const char* path, int oflag, ...) {
    // fake it for now
    return 0;
}

extern "C"
void abort() {
    while (true) {
        // do thing
    }
    // do nothing
}
extern "C"
off_t lseek(int fildes, off_t offset, int whence) {
    return 0;
}

extern "C"
int fstat(int fildes, struct stat* buf) {
   return 0;
}


extern "C"
void* sbrk(intptr_t increment) {
    return 0;
}

extern "C"
int close(int fildes) {
    return 0;
}

extern "C"
int isatty(int fd) {
    return 0;
}
#endif
