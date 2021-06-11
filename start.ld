/* 
 * TODO: adapt for my purposes 
 *
 */
GROUP(-lgcc -lm -lc -liberty -lc -lg2c)
MEMORY
{
    rom (rx)  : ORIGIN = 0, l = 512M
    ram (!rx) : o = 0x80000000, l = 512M
}

STACK_SIZE = 0x10000;

SECTIONS
{
    .text :
    {

    } >rom
    .bss :
    {

    } >ram
    .data :
    {
        __data_start__ = .;
        KEEP(*(.data))
        __data_end__ = .;
    } >ram
    .heap : {
        __heap_start__ = .;
        end = __heap_start__;
        _end = end;
        __end = end;
        KEEP(*(.heap))
        __heap_end__ = .;
        /* __HeapLimit = __heap_end__; */
    } >ram
}


cs1 = -(system_address_table + prcb_ptr + start_ip) ;

/* STARTUP ( CMakeFiles/hitagimon.dir/sx_init.s.o ) */
/*
How to dump the actual boot code:

i960-elf-objcopy --only-section=.text -O binary hitagimon hitagimon.bin

You have to make sure that you don't dump out the data section because that can get very large!
*/