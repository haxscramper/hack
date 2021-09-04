import std/[strformat]

var oPCODES_CYCLES = [
  4.uint8, 10, 7,  5,  5,  5,  7,  4,  4,  10, 7,  5,  5,  5,
  7,       4,  4,  10, 7,  5,  5,  5,  7,  4,  4,  10, 7,  5,
  5,       5,  7,  4,  4,  10, 16, 5,  5,  5,  7,  4,  4,  10,
  16,      5,  5,  5,  7,  4,  4,  10, 13, 5,  10, 10, 10,
  4,       4,  10, 13, 5,  5,  5,  7,  4,  5,  5,  5,  5,  5,
  5,       7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,
  5,       5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,
  5,       5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  7,  7,
  7,       7,  7,  7,  7,  7,  5,  5,  5,  5,  5,  5,  7,  5,  4,
  4,       4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
  4,       4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,
  4,       4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,
  7,       4,  4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,
  4,       7,  4,  5,  10, 10, 10, 11, 11, 7,  11, 5,
  10,      10, 10, 11, 17, 7,  11, 5,  10, 10, 10,
  11,      11, 7,  11, 5,  10, 10, 10, 11, 17, 7,  11,
  5,       10, 10, 18, 11, 11, 7,  11, 5,  5,  10, 4,
  11,      17, 7,  11, 5,  10, 10, 4,  11, 11, 7,  11,
  5,       5,  10, 4,  11, 17, 7,  11,
]


var dISASSEMBLE_TABLE = ["nop", "lxi b,#", "stax b", "inx b",
    "inr b", "dcr b", "mvi b,#", "rlc", "ill", "dad b", "ldax b", "dcx b",
    "inr c", "dcr c", "mvi c,#", "rrc", "ill", "lxi d,#", "stax d", "inx d",
    "inr d", "dcr d", "mvi d,#", "ral", "ill", "dad d", "ldax d", "dcx d",
    "inr e", "dcr e", "mvi e,#", "rar", "ill", "lxi h,#", "shld", "inx h",
    "inr h", "dcr h", "mvi h,#", "daa", "ill", "dad h", "lhld", "dcx h",
    "inr l", "dcr l", "mvi l,#", "cma", "ill", "lxi sp,#", "sta $", "inx sp",
    "inr M", "dcr M", "mvi M,#", "stc", "ill", "dad sp", "lda $", "dcx sp",
    "inr a", "dcr a", "mvi a,#", "cmc", "mov b,b", "mov b,c", "mov b,d",
    "mov b,e", "mov b,h", "mov b,l", "mov b,M", "mov b,a", "mov c,b", "mov c,c",
    "mov c,d", "mov c,e", "mov c,h", "mov c,l", "mov c,M", "mov c,a", "mov d,b",
    "mov d,c", "mov d,d", "mov d,e", "mov d,h", "mov d,l", "mov d,M", "mov d,a",
    "mov e,b", "mov e,c", "mov e,d", "mov e,e", "mov e,h", "mov e,l", "mov e,M",
    "mov e,a", "mov h,b", "mov h,c", "mov h,d", "mov h,e", "mov h,h", "mov h,l",
    "mov h,M", "mov h,a", "mov l,b", "mov l,c", "mov l,d", "mov l,e", "mov l,h",
    "mov l,l", "mov l,M", "mov l,a", "mov M,b", "mov M,c", "mov M,d", "mov M,e",
    "mov M,h", "mov M,l", "hlt", "mov M,a", "mov a,b", "mov a,c", "mov a,d",
    "mov a,e", "mov a,h", "mov a,l", "mov a,M", "mov a,a", "add b", "add c",
    "add d", "add e", "add h", "add l", "add M", "add a", "adc b", "adc c",
    "adc d", "adc e", "adc h", "adc l", "adc M", "adc a", "sub b", "sub c",
    "sub d", "sub e", "sub h", "sub l", "sub M", "sub a", "sbb b", "sbb c",
    "sbb d", "sbb e", "sbb h", "sbb l", "sbb M", "sbb a", "ana b", "ana c",
    "ana d", "ana e", "ana h", "ana l", "ana M", "ana a", "xra b", "xra c",
    "xra d", "xra e", "xra h", "xra l", "xra M", "xra a", "ora b", "ora c",
    "ora d", "ora e", "ora h", "ora l", "ora M", "ora a", "cmp b", "cmp c",
    "cmp d", "cmp e", "cmp h", "cmp l", "cmp M", "cmp a", "rnz", "pop b",
    "jnz $", "jmp $", "cnz $", "push b", "adi #", "rst 0", "rz", "ret", "jz $",
    "ill", "cz $", "call $", "aci #", "rst 1", "rnc", "pop d", "jnc $", "out p",
    "cnc $", "push d", "sui #", "rst 2", "rc", "ill", "jc $", "in p", "cc $",
    "ill", "sbi #", "rst 3", "rpo", "pop h", "jpo $", "xthl", "cpo $", "push h",
    "ani #", "rst 4", "rpe", "pchl", "jpe $", "xchg", "cpe $", "ill", "xri #",
    "rst 5", "rp", "pop psw", "jp $", "di", "cp $", "push psw", "ori #",
    "rst 6", "rm", "sphl", "jm $", "ei", "cm $", "ill", "cpi #", "rst 7"]

type
  i8080 = object
    readByte: proc(mem: pointer, memAddr: uint16): uint8
    writeByte: proc(mem: pointer, memAddr: uint16, value: uint8)
    portIn: proc(mem: pointer, port: uint8): uint8
    portOut: proc(mem: pointer, port: uint8, value: uint8)
    userdata: pointer
    cyc: uint64

    pc, sp: uint16
    a, b, c, d, e, h, l: uint8
    sf {.bitsize:1.}, zf {.bitsize:1.}, hf {.bitsize:1.}, pf {.bitsize:1.}, cf {.bitsize:1.}, iff {.bitsize:1.}: bool
    halted {.bitsize:1.}: bool
    interrupt_pending {.bitsize:1.}: bool

    interrupt_vector: uint8
    interrupt_delay: uint8

proc i8080_rb*(c: ptr i8080; aAddr: uint16): uint8 =
  return c.read_byte(c.userdata, aAddr)

proc i8080_wb*(c: ptr i8080; aAddr: uint16; val: uint8): void =
  c.write_byte(c.userdata, aAddr, val)

proc i8080_rw*(c: ptr i8080; aAddr: uint16): uint16 =
  return c.read_byte(c.userdata, aAddr + 1) shl 8 or
      c.read_byte(c.userdata, aAddr)

proc i8080_ww*(c: ptr i8080; aAddr: uint16; val: uint16): void =
  c.write_byte(c.userdata, aAddr, uint8(val and 0xFF))
  c.write_byte(c.userdata, aAddr + 1, uint8(val shr 8))

proc i8080_next_byte*(c: ptr i8080): uint8 =
  result = i8080_rb(c, c.pc)
  inc c.pc

proc i8080_next_word*(c: ptr i8080): uint16 =
  result = i8080_rw(c, c.pc)
  c.pc += 2

proc i8080_set_bc*(c: ptr i8080; val: uint16): void =
  c.b = uint8(val shr 8)
  c.c = uint8(val and 0xFF)

proc i8080_set_de*(c: ptr i8080; val: uint16): void =
  c.d = uint8(val shr 8)
  c.e = uint8(val and 0xFF)

proc i8080_set_hl*(c: ptr i8080; val: uint16): void =
  c.h = uint8(val shr 8)
  c.l = uint8(val and 0xFF)

proc i8080_get_bc*(c: ptr i8080): uint16 =
  return (c.b.uint16 shl 8) or c.c

proc i8080_get_de*(c: ptr i8080): uint16 =
  return (c.d.uint16 shl 8) or c.e

proc i8080_get_hl*(c: ptr i8080): uint16 =
  return (c.h.uint16 shl 8) or c.l

proc i8080_push_stack*(c: ptr i8080; val: uint16): void =
  c.sp -= 2
  i8080_ww(c, c.sp, val)

proc i8080_pop_stack*(c: ptr i8080): uint16 =
  var val: uint16 = i8080_rw(c, c.sp)
  c.sp += 2
  return val

proc parity*(val: uint8): bool =
  var nb_one_bits: uint8 = 0
  block:
    var i: cint = 0
    while i < 8:
      nb_one_bits += ((val shr i) and 1)
      inc i
  return (nb_one_bits and 1) == 0

proc carry*(bit_no: cint; a: uint8; b: uint8; cy: bool): bool =
  var res: int16 = int16(a) + int16(b) + int8(cy)
  var carry: int16 = res xor int16(a) xor int16(b)
  return bool(carry and (1 shl bit_no))

template setZsp(c, val): untyped =
  block:
    c.zf = (val) == 0
    c.sf = bool((val) shr 7)
    c.pf = parity(val)


proc i8080_add*(c: ptr i8080; reg: ptr uint8; val: uint8; cy: bool): void =
  var res: uint8 = reg[] + val + uint8(cy)
  c.cf = carry(8, reg[], val, cy)
  c.hf = carry(4, reg[], val, cy)
  sET_ZSP(c, res)
  reg[] = res

proc i8080_sub*(c: ptr i8080; reg: ptr uint8; val: uint8; cy: bool): void =
  i8080_add(c, reg, not(val), not(cy))
  c.cf = not(c.cf)

proc i8080_dad*(c: ptr i8080; val: uint16): void =
  c.cf = bool(((i8080_get_hl(c) + val) shr 16) and 1)
  i8080_set_hl(c, i8080_get_hl(c) + val)

proc i8080_inr*(c: ptr i8080; val: uint8): uint8 =
  result = val + 1
  c.hf = ((result and 0xF) == 0)
  sET_ZSP(c, result)

proc i8080_dcr*(c: ptr i8080; val: uint8): uint8 =
  result = val - 1
  c.hf = not(((result and 0xF) == 0xF))
  sET_ZSP(c, result)

proc i8080_ana*(c: ptr i8080; val: uint8): void =
  var result: uint8 = c.a and val
  c.cf = bool(0)
  c.hf = (((c.a or val) and 0x08) != 0)
  sET_ZSP(c, result)
  c.a = result

proc i8080_xra*(c: ptr i8080; val: uint8): void =
  c.a = val xor c.a
  c.cf = false
  c.hf = false
  sET_ZSP(c, c.a)

proc i8080_ora*(c: ptr i8080; val: uint8): void =
  c.a = val or c.a
  c.cf = false
  c.hf = false
  sET_ZSP(c, c.a)

proc i8080_cmp*(c: ptr i8080; val: uint8): void =
  var result: int16 = int16(c.a - val)
  c.cf = bool(result shr 8)
  c.hf = bool(not((c.a.int16 xor result xor val.int16)).uint8 and 0x10)
  sET_ZSP(c, result.uint8 and 0xFF)

proc i8080_jmp*(c: ptr i8080; aAddr: uint16): void =
  c.pc = aAddr

proc i8080_cond_jmp*(c: ptr i8080; condition: bool): void =
  var aAddr: uint16 = i8080_next_word(c)
  if condition:
    c.pc = aAddr

proc i8080_call*(c: ptr i8080; aAddr: uint16): void =
  i8080_push_stack(c, c.pc)
  i8080_jmp(c, aAddr)

proc i8080_cond_call*(c: ptr i8080; condition: bool): void =
  var aAddr: uint16 = i8080_next_word(c)
  if condition:
    i8080_call(c, aAddr)
    c.cyc += 6

proc i8080_ret*(c: ptr i8080): void =
  c.pc = i8080_pop_stack(c)

proc i8080_cond_ret*(c: ptr i8080; condition: bool): void =
  if condition:
    i8080_ret(c)
    c.cyc += 6

proc i8080_push_psw*(c: ptr i8080): void =
  var psw: uint8 = 0
  psw = c.sf.uint8 shl 7 or psw
  psw = c.zf.uint8 shl 6 or psw
  psw = c.hf.uint8 shl 4 or psw
  psw = c.pf.uint8 shl 2 or psw
  psw = 1 shl 1 or psw
  psw = c.cf.uint8 shl 0 or psw
  i8080_push_stack(c, c.a shl 8 or psw)

proc i8080_pop_psw*(c: ptr i8080): void =
  var af: uint16 = i8080_pop_stack(c)
  c.a = uint8(af shr 8)
  var psw: uint8 = uint8(af and 0xFF)
  c.sf = bool((psw shr 7) and 1)
  c.zf = bool((psw shr 6) and 1)
  c.hf = bool((psw shr 4) and 1)
  c.pf = bool((psw shr 2) and 1)
  c.cf = bool((psw shr 0) and 1)

proc i8080_rlc*(c: ptr i8080): void =
  c.cf = bool(c.a.uint8 shr 7)
  c.a = ((c.a.uint8 shl 1) or c.cf.uint8)

proc i8080_rrc*(c: ptr i8080): void =
  c.cf = bool(c.a.uint8 and 1)
  c.a = ((c.a.uint8 shr 1) or uint8(c.cf.uint8 shl 7))

proc i8080_ral*(c: ptr i8080): void =
  var cy: bool = c.cf
  c.cf = bool(c.a.uint8 shr 7)
  c.a = ((c.a.uint8 shl 1) or cy.uint8)

proc i8080_rar*(c: ptr i8080): void =
  var cy: bool = c.cf
  c.cf = bool(c.a.uint8 and 1)
  c.a = ((c.a.uint8 shr 1) or (cy.uint8 shl 7))

proc i8080_daa*(c: ptr i8080): void =
  var cy: bool = c.cf
  var correction: uint8 = 0
  var lsb: uint8 = c.a and 0x0F
  var msb: uint8 = c.a shr 4
  if c.hf or (lsb > 9):
    correction += 0x06
  if c.cf or (msb > 9) or ((msb >= 9) and (lsb > 9)):
    correction += 0x60
    cy = true
  i8080_add(c, addr c.a, correction, false)
  c.cf = cy

proc i8080_xchg*(c: ptr i8080): void =
  var de: uint16 = i8080_get_de(c)
  i8080_set_de(c, i8080_get_hl(c))
  i8080_set_hl(c, de)

proc i8080_xthl*(c: ptr i8080): void =
  var val: uint16 = i8080_rw(c, c.sp)
  i8080_ww(c, c.sp, i8080_get_hl(c))
  i8080_set_hl(c, val)

proc i8080_execute*(c: ptr i8080; opcode: uint8): void =
  c.cyc += oPCODES_CYCLES[opcode]
  if c.interrupt_delay > 0:
    c.interrupt_delay -= 1
  case opcode
  of 0x7F:
    c.a = c.a
  of 0x78:
    c.a = c.b
  of 0x79:
    c.a = c.c
  of 0x7A:
    c.a = c.d
  of 0x7B:
    c.a = c.e
  of 0x7C:
    c.a = c.h
  of 0x7D:
    c.a = c.l
  of 0x7E:
    c.a = i8080_rb(c, i8080_get_hl(c))
  of 0x0A:
    c.a = i8080_rb(c, i8080_get_bc(c))
  of 0x1A:
    c.a = i8080_rb(c, i8080_get_de(c))
  of 0x3A:
    c.a = i8080_rb(c, i8080_next_word(c))
  of 0x47:
    c.b = c.a
  of 0x40:
    c.b = c.b
  of 0x41:
    c.b = c.c
  of 0x42:
    c.b = c.d
  of 0x43:
    c.b = c.e
  of 0x44:
    c.b = c.h
  of 0x45:
    c.b = c.l
  of 0x46:
    c.b = i8080_rb(c, i8080_get_hl(c))
  of 0x4F:
    c.c = c.a
  of 0x48:
    c.c = c.b
  of 0x49:
    c.c = c.c
  of 0x4A:
    c.c = c.d
  of 0x4B:
    c.c = c.e
  of 0x4C:
    c.c = c.h
  of 0x4D:
    c.c = c.l
  of 0x4E:
    c.c = i8080_rb(c, i8080_get_hl(c))
  of 0x57:
    c.d = c.a
  of 0x50:
    c.d = c.b
  of 0x51:
    c.d = c.c
  of 0x52:
    c.d = c.d
  of 0x53:
    c.d = c.e
  of 0x54:
    c.d = c.h
  of 0x55:
    c.d = c.l
  of 0x56:
    c.d = i8080_rb(c, i8080_get_hl(c))
  of 0x5F:
    c.e = c.a
  of 0x58:
    c.e = c.b
  of 0x59:
    c.e = c.c
  of 0x5A:
    c.e = c.d
  of 0x5B:
    c.e = c.e
  of 0x5C:
    c.e = c.h
  of 0x5D:
    c.e = c.l
  of 0x5E:
    c.e = i8080_rb(c, i8080_get_hl(c))
  of 0x67:
    c.h = c.a
  of 0x60:
    c.h = c.b
  of 0x61:
    c.h = c.c
  of 0x62:
    c.h = c.d
  of 0x63:
    c.h = c.e
  of 0x64:
    c.h = c.h
  of 0x65:
    c.h = c.l
  of 0x66:
    c.h = i8080_rb(c, i8080_get_hl(c))
  of 0x6F:
    c.l = c.a
  of 0x68:
    c.l = c.b
  of 0x69:
    c.l = c.c
  of 0x6A:
    c.l = c.d
  of 0x6B:
    c.l = c.e
  of 0x6C:
    c.l = c.h
  of 0x6D:
    c.l = c.l
  of 0x6E:
    c.l = i8080_rb(c, i8080_get_hl(c))
  of 0x77:
    i8080_wb(c, i8080_get_hl(c), c.a)
  of 0x70:
    i8080_wb(c, i8080_get_hl(c), c.b)
  of 0x71:
    i8080_wb(c, i8080_get_hl(c), c.c)
  of 0x72:
    i8080_wb(c, i8080_get_hl(c), c.d)
  of 0x73:
    i8080_wb(c, i8080_get_hl(c), c.e)
  of 0x74:
    i8080_wb(c, i8080_get_hl(c), c.h)
  of 0x75:
    i8080_wb(c, i8080_get_hl(c), c.l)
  of 0x3E:
    c.a = i8080_next_byte(c)
  of 0x06:
    c.b = i8080_next_byte(c)
  of 0x0E:
    c.c = i8080_next_byte(c)
  of 0x16:
    c.d = i8080_next_byte(c)
  of 0x1E:
    c.e = i8080_next_byte(c)
  of 0x26:
    c.h = i8080_next_byte(c)
  of 0x2E:
    c.l = i8080_next_byte(c)
  of 0x36:
    i8080_wb(c, i8080_get_hl(c), i8080_next_byte(c))
  of 0x02:
    i8080_wb(c, i8080_get_bc(c), c.a)
  of 0x12:
    i8080_wb(c, i8080_get_de(c), c.a)
  of 0x32:
    i8080_wb(c, i8080_next_word(c), c.a)
  of 0x01:
    i8080_set_bc(c, i8080_next_word(c))
  of 0x11:
    i8080_set_de(c, i8080_next_word(c))
  of 0x21:
    i8080_set_hl(c, i8080_next_word(c))
  of 0x31:
    c.sp = i8080_next_word(c)
  of 0x2A:
    i8080_set_hl(c, i8080_rw(c, i8080_next_word(c)))
  of 0x22:
    i8080_ww(c, i8080_next_word(c), i8080_get_hl(c))
  of 0xF9:
    c.sp = i8080_get_hl(c)
  of 0xEB:
    i8080_xchg(c)
  of 0xE3:
    i8080_xthl(c)
  of 0x87:
    i8080_add(c, addr c.a, c.a, false)
  of 0x80:
    i8080_add(c, addr c.a, c.b, false)
  of 0x81:
    i8080_add(c, addr c.a, c.c, false)
  of 0x82:
    i8080_add(c, addr c.a, c.d, false)
  of 0x83:
    i8080_add(c, addr c.a, c.e, false)
  of 0x84:
    i8080_add(c, addr c.a, c.h, false)
  of 0x85:
    i8080_add(c, addr c.a, c.l, false)
  of 0x86:
    i8080_add(c, addr c.a, i8080_rb(c, i8080_get_hl(c)), false)
  of 0xC6:
    i8080_add(c, addr c.a, i8080_next_byte(c), false)
  of 0x8F:
    i8080_add(c, addr c.a, c.a, c.cf)
  of 0x88:
    i8080_add(c, addr c.a, c.b, c.cf)
  of 0x89:
    i8080_add(c, addr c.a, c.c, c.cf)
  of 0x8A:
    i8080_add(c, addr c.a, c.d, c.cf)
  of 0x8B:
    i8080_add(c, addr c.a, c.e, c.cf)
  of 0x8C:
    i8080_add(c, addr c.a, c.h, c.cf)
  of 0x8D:
    i8080_add(c, addr c.a, c.l, c.cf)
  of 0x8E:
    i8080_add(c, addr c.a, i8080_rb(c, i8080_get_hl(c)), c.cf)
  of 0xCE:
    i8080_add(c, addr c.a, i8080_next_byte(c), c.cf)
  of 0x97:
    i8080_sub(c, addr c.a, c.a, false)
  of 0x90:
    i8080_sub(c, addr c.a, c.b, false)
  of 0x91:
    i8080_sub(c, addr c.a, c.c, false)
  of 0x92:
    i8080_sub(c, addr c.a, c.d, false)
  of 0x93:
    i8080_sub(c, addr c.a, c.e, false)
  of 0x94:
    i8080_sub(c, addr c.a, c.h, false)
  of 0x95:
    i8080_sub(c, addr c.a, c.l, false)
  of 0x96:
    i8080_sub(c, addr c.a, i8080_rb(c, i8080_get_hl(c)), false)
  of 0xD6:
    i8080_sub(c, addr c.a, i8080_next_byte(c), false)
  of 0x9F:
    i8080_sub(c, addr c.a, c.a, c.cf)
  of 0x98:
    i8080_sub(c, addr c.a, c.b, c.cf)
  of 0x99:
    i8080_sub(c, addr c.a, c.c, c.cf)
  of 0x9A:
    i8080_sub(c, addr c.a, c.d, c.cf)
  of 0x9B:
    i8080_sub(c, addr c.a, c.e, c.cf)
  of 0x9C:
    i8080_sub(c, addr c.a, c.h, c.cf)
  of 0x9D:
    i8080_sub(c, addr c.a, c.l, c.cf)
  of 0x9E:
    i8080_sub(c, addr c.a, i8080_rb(c, i8080_get_hl(c)), c.cf)
  of 0xDE:
    i8080_sub(c, addr c.a, i8080_next_byte(c), c.cf)
  of 0x09:
    i8080_dad(c, i8080_get_bc(c))
  of 0x19:
    i8080_dad(c, i8080_get_de(c))
  of 0x29:
    i8080_dad(c, i8080_get_hl(c))
  of 0x39:
    i8080_dad(c, c.sp)
  of 0xF3:
    c.iff = false
  of 0xFB:
    c.iff = true
    c.interrupt_delay = 1
  of 0x00:
    discard
  of 0x76:
    c.halted = true
  of 0x3C:
    c.a = i8080_inr(c, c.a)
  of 0x04:
    c.b = i8080_inr(c, c.b)
  of 0x0C:
    c.c = i8080_inr(c, c.c)
  of 0x14:
    c.d = i8080_inr(c, c.d)
  of 0x1C:
    c.e = i8080_inr(c, c.e)
  of 0x24:
    c.h = i8080_inr(c, c.h)
  of 0x2C:
    c.l = i8080_inr(c, c.l)
  of 0x34:
    i8080_wb(c, i8080_get_hl(c), i8080_inr(c, i8080_rb(c, i8080_get_hl(c))))
  of 0x3D:
    c.a = i8080_dcr(c, c.a)
  of 0x05:
    c.b = i8080_dcr(c, c.b)
  of 0x0D:
    c.c = i8080_dcr(c, c.c)
  of 0x15:
    c.d = i8080_dcr(c, c.d)
  of 0x1D:
    c.e = i8080_dcr(c, c.e)
  of 0x25:
    c.h = i8080_dcr(c, c.h)
  of 0x2D:
    c.l = i8080_dcr(c, c.l)
  of 0x35:
    i8080_wb(c, i8080_get_hl(c), i8080_dcr(c, i8080_rb(c, i8080_get_hl(c))))
  of 0x03:
    i8080_set_bc(c, i8080_get_bc(c) + 1)
  of 0x13:
    i8080_set_de(c, i8080_get_de(c) + 1)
  of 0x23:
    i8080_set_hl(c, i8080_get_hl(c) + 1)
  of 0x33:
    c.sp += 1
  of 0x0B:
    i8080_set_bc(c, i8080_get_bc(c) - 1)
  of 0x1B:
    i8080_set_de(c, i8080_get_de(c) - 1)
  of 0x2B:
    i8080_set_hl(c, i8080_get_hl(c) - 1)
  of 0x3B:
    c.sp -= 1
  of 0x27:
    i8080_daa(c)
  of 0x2F:
    c.a = not(c.a)
  of 0x37:
    c.cf = true
  of 0x3F:
    c.cf = not(c.cf)
  of 0x07:
    i8080_rlc(c)
  of 0x0F:
    i8080_rrc(c)
  of 0x17:
    i8080_ral(c)
  of 0x1F:
    i8080_rar(c)
  of 0xA7:
    i8080_ana(c, c.a)
  of 0xA0:
    i8080_ana(c, c.b)
  of 0xA1:
    i8080_ana(c, c.c)
  of 0xA2:
    i8080_ana(c, c.d)
  of 0xA3:
    i8080_ana(c, c.e)
  of 0xA4:
    i8080_ana(c, c.h)
  of 0xA5:
    i8080_ana(c, c.l)
  of 0xA6:
    i8080_ana(c, i8080_rb(c, i8080_get_hl(c)))
  of 0xE6:
    i8080_ana(c, i8080_next_byte(c))
  of 0xAF:
    i8080_xra(c, c.a)
  of 0xA8:
    i8080_xra(c, c.b)
  of 0xA9:
    i8080_xra(c, c.c)
  of 0xAA:
    i8080_xra(c, c.d)
  of 0xAB:
    i8080_xra(c, c.e)
  of 0xAC:
    i8080_xra(c, c.h)
  of 0xAD:
    i8080_xra(c, c.l)
  of 0xAE:
    i8080_xra(c, i8080_rb(c, i8080_get_hl(c)))
  of 0xEE:
    i8080_xra(c, i8080_next_byte(c))
  of 0xB7:
    i8080_ora(c, c.a)
  of 0xB0:
    i8080_ora(c, c.b)
  of 0xB1:
    i8080_ora(c, c.c)
  of 0xB2:
    i8080_ora(c, c.d)
  of 0xB3:
    i8080_ora(c, c.e)
  of 0xB4:
    i8080_ora(c, c.h)
  of 0xB5:
    i8080_ora(c, c.l)
  of 0xB6:
    i8080_ora(c, i8080_rb(c, i8080_get_hl(c)))
  of 0xF6:
    i8080_ora(c, i8080_next_byte(c))
  of 0xBF:
    i8080_cmp(c, c.a)
  of 0xB8:
    i8080_cmp(c, c.b)
  of 0xB9:
    i8080_cmp(c, c.c)
  of 0xBA:
    i8080_cmp(c, c.d)
  of 0xBB:
    i8080_cmp(c, c.e)
  of 0xBC:
    i8080_cmp(c, c.h)
  of 0xBD:
    i8080_cmp(c, c.l)
  of 0xBE:
    i8080_cmp(c, i8080_rb(c, i8080_get_hl(c)))
  of 0xFE:
    i8080_cmp(c, i8080_next_byte(c))
  of 0xC3:
    i8080_jmp(c, i8080_next_word(c))
  of 0xC2:
    i8080_cond_jmp(c, c.zf == false)
  of 0xCA:
    i8080_cond_jmp(c, c.zf == true)
  of 0xD2:
    i8080_cond_jmp(c, c.cf == false)
  of 0xDA:
    i8080_cond_jmp(c, c.cf == true)
  of 0xE2:
    i8080_cond_jmp(c, c.pf == false)
  of 0xEA:
    i8080_cond_jmp(c, c.pf == true)
  of 0xF2:
    i8080_cond_jmp(c, c.sf == false)
  of 0xFA:
    i8080_cond_jmp(c, c.sf == true)
  of 0xE9:
    c.pc = i8080_get_hl(c)
  of 0xCD:
    i8080_call(c, i8080_next_word(c))
  of 0xC4:
    i8080_cond_call(c, c.zf == false)
  of 0xCC:
    i8080_cond_call(c, c.zf == true)
  of 0xD4:
    i8080_cond_call(c, c.cf == false)
  of 0xDC:
    i8080_cond_call(c, c.cf == true)
  of 0xE4:
    i8080_cond_call(c, c.pf == false)
  of 0xEC:
    i8080_cond_call(c, c.pf == true)
  of 0xF4:
    i8080_cond_call(c, c.sf == false)
  of 0xFC:
    i8080_cond_call(c, c.sf == true)
  of 0xC9:
    i8080_ret(c)
  of 0xC0:
    i8080_cond_ret(c, c.zf == false)
  of 0xC8:
    i8080_cond_ret(c, c.zf == true)
  of 0xD0:
    i8080_cond_ret(c, c.cf == false)
  of 0xD8:
    i8080_cond_ret(c, c.cf == true)
  of 0xE0:
    i8080_cond_ret(c, c.pf == false)
  of 0xE8:
    i8080_cond_ret(c, c.pf == true)
  of 0xF0:
    i8080_cond_ret(c, c.sf == false)
  of 0xF8:
    i8080_cond_ret(c, c.sf == true)
  of 0xC7:
    i8080_call(c, 0x00)
  of 0xCF:
    i8080_call(c, 0x08)
  of 0xD7:
    i8080_call(c, 0x10)
  of 0xDF:
    i8080_call(c, 0x18)
  of 0xE7:
    i8080_call(c, 0x20)
  of 0xEF:
    i8080_call(c, 0x28)
  of 0xF7:
    i8080_call(c, 0x30)
  of 0xFF:
    i8080_call(c, 0x38)
  of 0xC5:
    i8080_push_stack(c, i8080_get_bc(c))
  of 0xD5:
    i8080_push_stack(c, i8080_get_de(c))
  of 0xE5:
    i8080_push_stack(c, i8080_get_hl(c))
  of 0xF5:
    i8080_push_psw(c)
  of 0xC1:
    i8080_set_bc(c, i8080_pop_stack(c))
  of 0xD1:
    i8080_set_de(c, i8080_pop_stack(c))
  of 0xE1:
    i8080_set_hl(c, i8080_pop_stack(c))
  of 0xF1:
    i8080_pop_psw(c)
  of 0xDB:
    c.a = c.port_in(c.userdata, i8080_next_byte(c))
  of 0xD3:
    c.port_out(c.userdata, i8080_next_byte(c), c.a)
  of 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38:
    discard
  of 0xD9:
    i8080_ret(c)
  of 0xDD, 0xED, 0xFD:
    i8080_call(c, i8080_next_word(c))
  of 0xCB:
    i8080_jmp(c, i8080_next_word(c))

proc i8080_init*(c: ptr i8080): void =
  c.read_byte = nil
  c.write_byte = nil
  c.port_in = nil
  c.port_out = nil
  c.userdata = nil
  c.cyc = 0
  c.pc = 0
  c.sp = 0
  c.a = 0
  c.b = 0
  c.c = 0
  c.d = 0
  c.e = 0
  c.h = 0
  c.l = 0
  c.sf = false
  c.zf = false
  c.hf = false
  c.pf = false
  c.cf = false
  c.iff = false
  c.halted = false
  c.interrupt_pending = false
  c.interrupt_vector = 0
  c.interrupt_delay = 0

proc i8080_step*(c: ptr i8080): void =
  if c.interrupt_pending and c.iff and (c.interrupt_delay == 0):
    c.interrupt_pending = false
    c.iff = false
    c.halted = false
    i8080_execute(c, c.interrupt_vector)

proc i8080_interrupt*(c: ptr i8080; opcode: uint8): void =
  c.interrupt_pending = true
  c.interrupt_vector = opcode

proc i8080_debug_output*(c: ptr i8080; print_disassembly: bool): void =
  var f: uint8 = 0
  f = c.sf.uint8 shl 7 or f
  f = c.zf.uint8 shl 6 or f
  f = c.hf.uint8 shl 4 or f
  f = c.pf.uint8 shl 2 or f
  f =          1 shl 1 or f
  f = c.cf.uint8 shl 0 or f

  echo fmt"PC: {c.pc}, AF: {c.a shl 8 or f}, BC: {i8080_get_bc(c)}, ",
       fmt"DE: {i8080_get_de(c)}, HL: {i8080_get_hl(c)}, SP: {c.sp}, CYC: {c.cyc}",
       fmt"({i8080_rb(c, c.pc)} {i8080_rb(c, c.pc + 1)} {i8080_rb(c, c.pc + 2)} {i8080_rb(c, c.pc + 3)})"

var memory: seq[uint8]
var test_finished: bool = false
proc rb*(userdata: pointer; aAddr: uint16): uint8 =
  return memory[aAddr]

proc wb*(userdata: pointer; aAddr: uint16; val: uint8): void =
  memory[aAddr] = val

proc port_in*(userdata: pointer; port: uint8): uint8 =
  return 0x00

proc port_out*(userdata: pointer; port: uint8; value: uint8): void =
  var c: ptr i8080 = cast[ptr i8080](userdata)
  if port == 0:
    test_finished = true


proc run_test*(c: ptr i8080; filename: string; cyc_expected: culong): void =
  i8080_init(c)
  c.userdata = c
  c.read_byte = rb
  c.write_byte = wb
  c.port_in = port_in
  c.port_out = port_out
  c.pc = 0x100
  memory[0x0000] = 0xD3
  memory[0x0001] = 0x00
  memory[0x0005] = 0xD3
  memory[0x0006] = 0x01
  memory[0x0007] = 0xC9
  var nb_instructions: clong = 0
  test_finished = false
  while (not(test_finished)):
    nb_instructions += 1
    i8080_step(c)
  var diff = cyc_expected - c.cyc

proc main*() =
  var cpu: i8080
