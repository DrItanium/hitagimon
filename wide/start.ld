/* GROUP(-lgcc -lm -lc -liberty -lc -lmon960) */ /* libraries to link in */

MEMORY /* describe the partitioning of the memory space for the application */ 
{
    rom (!r) : ORIGIN = 0, l = 128k,
    ram (!rx) : ORIGIN = (16M+1M), l = 128k
}


/* Layout the different ELF sections into memory using the above described
 * memory spaces */ 
SECTIONS
{
	/* this is the text section */ 
	.text : 
	{ 
		CREATE_OBJECT_SYMBOLS /* C++ needs this */ 

		*(.text) 	/* place the actual contents of the text section here */ 

		_etext = .; /* define a symbol named _etext and assign it to the
					   address following the end of the text section data */ 

	/* the arrow syntax tells the linker that the text section will go into the
	 * memory location named "rom", it is sequential as we will see in the next
	 * few sections*/ 
	} >rom
	/* setup the bss section, tag it's start and end positions to allow the
	 * i960 to manually zero out the memory on the sdcard's ram.bin file */ 
	.bss : 
	{ 
		__bss_start__ = .;
		_bss_start = .;
		*(.bss) 
		*(COMMON) /* can't find much documentation on this but it is a good idea to have this here */ 
		__bss_end__ = .; 
	} >ram 
	.data :
	{
		__data_start__ = .; 
		KEEP(*(.data)) 
		CONSTRUCTORS  /* we want to place the constructors data here */
		__data_end__ = .; 
		_edata = .; 
	} >ram 
	.heap : { 
		/* this section is synthetic, I do not actually copy the contents of the heap
		 * section over on boot, instead I just want the location of where it should
		 * start.In this case, the heap will start _after_ the .bss and .data sections
		 * in ram. It is placed linearly in order so there is no mystery to this. 
		 */
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
handoff_iac =         0x01000020;
boot_img_length =     0x00100000;
boot_img_src_base =   0xFE800000;
boot_img_dest_base =  0x01000000;
serial_base_offset =  0xFE000008;
serial_flush_offset = 0xFE00000C;