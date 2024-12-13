GROUP(-lgcc -lm -lc -lmon960) /* libraries to link in */

MEMORY /* describe the partitioning of the memory space for the application */ 
{
    imi (!r) : ORIGIN = 0, l = 32,
    rom (!r) : ORIGIN = 32, l = 2M,
    psram(!rx) : ORIGIN = 2M, l = 62M,
    psram_upper(!rx) : ORIGIN = 64M, l = 2048M - 64M
}

/* Define a symbol that your code can reference. The linker adds the symbol. */ 
/* I am reducing the stacks to 32k instead of 64k because I don't have much space in 512k anymore */
STACK_SIZE = 0x8000;

/* Layout the different ELF sections into memory using the above described
 * memory spaces */ 
SECTIONS
{
    .boot :
    {
        *(.imi*)
    } > imi
	/* this is the text section */
	.text : 
	{
		*(.text*) 	/* place the actual contents of the text section here */
	    __rodata_start__ = .;
	    KEEP(*(.rodata*))
	    __rodata_end__ = .;
	    *(.gcc_except_table)

		_etext = .; /* define a symbol named _etext and assign it to the
					   address following the end of the text section data */ 

	/* the arrow syntax tells the linker that the text section will go into the
	 * memory location named "rom", it is sequential as we will see in the next
	 * few sections*/ 
	} >rom
	.init_array : {
		/* .init_array */
		PROVIDE_HIDDEN(__init_array_start = .);
		KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
		KEEP (*(.init array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o) .ctors))
		PROVIDE_HIDDEN(__init_array_end = .);
	} > psram
	.fini_array  : {
		/* .fini_array */
		PROVIDE_HIDDEN(__fini_array_start = .);
		KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
		KEEP (*(.init array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o) .dtors))
		PROVIDE_HIDDEN(__fini_array_end = .);
	} > psram
	.ctors : {
		/* According to the core linker script, gcc uses crtbegin.o to find the start of the constructors. So this
		   sequence ensures that it is first. */
	    KEEP (*crtbegin.o(.ctors))
	    KEEP (*crtbegin?.o(.ctors))
	    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o) .ctors))
	    KEEP (*(SORT(.ctors.*)))
	    KEEP (*(.ctors))
	} > psram
	.dtors : {
	    KEEP (*crtbegin.o(.dtors))
	    KEEP (*crtbegin?.o(.dtors))
	    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o) .dtors))
	    KEEP (*(SORT(.dtors.*)))
	    KEEP (*(.dtors))
    } > psram
	/*
	   Shove a bunch of different sections into data since we have to flatten this anyways.
	   Eventually, an ELF format file will be better accessed later on.
	   */
	.data :
	{
		__data_start__ = .; 
		KEEP(*(.data .data.* .gnu.linkonce.d.*))
		SORT(CONSTRUCTORS)  /* we want to place the constructors data here */
		__data_end__ = .;
	    /* normally this would be the .fini section */
	    KEEP (*(SORT_NONE(.fini)))
	} >psram
	_edata = .;
	. = .;
	__bss_start__ = .;
	bss_start = .;
	.sbss : {
	    *(.dynsbss)
	    *(.sbss .sbss.* .gnu.linkonce.sb.*)
	    *(.scommon)
	} >psram
	/* setup the bss section, tag it's start and end positions to allow the
	 * i960 to manually zero out the memory on the sdcard's ram.bin file */
	.bss :
	{
	    *(.dynbss)
		*(.bss .bss.* .gnu.linkonce.b.*)
		*(COMMON) /* can't find much documentation on this but it is a good idea to have this here */
		. = ALIGN(. != 0 ? 32 / 8 : 1);
	} >psram
	. = ALIGN(32 / 8);
	. = SEGMENT_START("ldata-segment", .);
	. = ALIGN(32 / 8);
	__bss_end__ = .;
	end = .;
	PROVIDE(end = .);
	.heap : {
		/* this section is synthetic, I do not actually copy the contents of the heap
		 * section over on boot, instead I just want the location of where it should
		 * start.In this case, the heap will start _after_ the .bss and .data sections
		 * in ram. It is placed linearly in order so there is no mystery to this. 
		 */
		__heap_start__ = .; 
		end = __heap_start__; 
		KEEP(*(.heap))
		__heap_end__ = .; 
		/* __HeapLimit = __heap_end__; */ 
	} >psram
}

/* these two locations are _very_ important for getting the program to startup correctly */ 



/* Currently, I am manually decomposing the elf binary into ROM (.text) and DAT
 * (.data) files from the ELF using objcopy.  

 I have the chipset map the data section to 0x2000'0000 so we need to
 reflect that in the linker script (start_ip in the sx_init.s uses this). 

 I also compute the data section's length ahead of time to save instructions.  



 It is the responsibility of the i960 to copy the contents of the mapped data
 section into ram starting at address 0x8000'0000 on startup. Failure to do so
 leads to the inability to properly allocate memory (no malloc) and other
 various oddities. 



 The variable cs1 is specific to the i960 and is used in it's boot process.
 The processor on startup loads the first eight words (32 bytes) and performs
 a check sum with them. The processor will refuse to boot if the checksum
 algorithm does not yield a zero from combining these eight words together.
 Thus cs1 is used to make sure that 0 is the final result. The i960 SA/SB
 reference manual is where I got the cs1 line 



 The BSS Section length is used on startup to "copy" the bss into ram like the
 data section (although BSS copy happens first because it comes before data)..
 In reality this just zeroing out ram on the SDCard (ram.bin) to prevent bad
 things from happening during program execution 

 */ 

cs1 = -(ABSOLUTE(system_address_table) + ABSOLUTE(prcb_ptr) + ABSOLUTE(start_ip)) ;
transferCacheSource = 0xFE800000;
transferCacheSize = 1024*1024;
transferCacheDest = 0xFE800000;

/* dest being the same as src will cause the transfer to be skipped */

/* This directive allows you to denote the file that will go onto the front of
 * the text section, the problem is that with CMake this moves around. So
 * instead, I just make sure that sx_init.s is the first file in the list of
 * dependencies in CMakeFiles.txt. It does the same thing as the linker
 * appplies objects on a first come first serve basis. Very nice! */ 

/* STARTUP ( CMakeFiles/hitagimon.dir/sx_init.s.o ) */ 

/*

How to dump the actual boot code: 


i960-elf-objcopy --only-section=.text -O binary hitagimon hitagimon.bin 


You have to make sure that you don't dump out the data section because that can
get very large! This is because, objcopy is turning the dynamic elf into a
statically mapped binary blob. Since ram starts at 0x8000'0000 the blob must
have ram mapped at that exact location for bss and data. Thus the blob becomes
over 2 gigabytes in size! This returns the baffling error "not enough space on
device" from i960-elf-objcopy.  

 */ 
