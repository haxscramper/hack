OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
TARGET(binary)
INPUT(_T3)
INPUT(_D3)

SECTIONS {
  .text : {
    _T3_start = .;
    "_T3"
    _T3_end = .;
  }
  .data : {
    _D3_start = .;
    "_D3"
    _D3_end = .;
  }
}
