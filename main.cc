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
    uint16_t previousColor = theDisplay.color565(0);
    for (int i = 0x8000; i < 0x1000000; ++i) {
        uint16_t color = theDisplay.color565(i);
        if (previousColor != color) {
            for (int x = 0; x < 64; ++x) {
                for (int y = 0; y < 64; ++y) {
                    for (int w = 1; w < 32; ++w) {
                       for (int h = 1; h < 32; ++h)  {
                           theDisplay.fillRect(x+128, y+128, w+64, h+64, color) ;
                           theDisplay.fillRect(x+64, y+64, w+32, h+32, color) ;
                           theDisplay.fillRect(x, y, w, h, color) ;
                       }
                    }
                }
            }
        }
        previousColor = color;
    }
    for (int i = 0x8000; i < 0x1000000; ++i) {
        uint16_t color = theDisplay.color565(i);
        if (previousColor != color) {
            for (int x = 0; x < 64; ++x) {
                for (int y = 0; y < 64; ++y) {
                    theDisplay.drawPixel(x, y, color);
                    theDisplay.drawPixel(x, y * 1, color);
                    theDisplay.drawPixel(x, y * 2, color);
                    theDisplay.drawPixel(x, y * 3, color);
                    theDisplay.drawPixel(x, y * 4, color);
                    theDisplay.drawPixel(x, y * 5, color);
                    theDisplay.drawPixel(x, y * 6, color);
                    theDisplay.drawPixel(x, y * 7, color);
                }
            }
        }
        previousColor = color;
    }
    while(true) {
        theLed.toggle();
        counter += wait(1000000);
        theLed.toggle();
        counter += wait(1000000);
    }
    return 0;
}
#if 0
extern "C"
int atexit(void (*function)(void))
{
     function();
}
// functions for back end testing
extern "C"
ssize_t write(int fildes, const void* buf, size_t nbyte) {
    const char* theBuf = (const char*)buf;
    for (size_t i = 0; i < nbyte; ++i) {
        theConsole.write(static_cast<uint16_t>(theBuf[i]));
    }
    return nbyte;
}

extern "C"
ssize_t read(int fildes, void* buf, size_t nbyte) {
    char* theBuf = (char*)buf;
    for (int i = 0; i < nbyte; ++i) {
        uint16_t value = static_cast<uint16_t>(theConsole.read());
        theBuf[i] = static_cast<char>(value);
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
int close(int fildes) {
    return 0;
}

extern "C"
int isatty(int fd) {
    return 0;
}
#endif
