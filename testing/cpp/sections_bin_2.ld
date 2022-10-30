OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
TARGET(binary)
INPUT(_T2)
INPUT(_D2)

SECTIONS {
  .text : {
    "_T2"
  }
  .data : {
    "_D2"
  }
}
