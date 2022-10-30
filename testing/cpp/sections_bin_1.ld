OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
TARGET(binary)
INPUT(_T1)
INPUT(_D1)

SECTIONS {
  .text : {
    "_T1"
  }
  .data : {
    "_D1"
  }
}
