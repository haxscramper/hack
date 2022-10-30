OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
SECTIONS {
 .text : {
   *(.text)
 }
 .data : {
   *(.data)
 }
}
