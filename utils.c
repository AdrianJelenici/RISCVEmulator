#include "utils.h"
#include <stdio.h>
#include <stdlib.h>

/* Sign extends the given field to a 32-bit integer where field is
 * interpreted an n-bit integer. */
int sign_extend_number(unsigned int field, unsigned int n) {
  /* YOUR CODE HERE */
  return (int)field << (32 - n) >> (32 - n);
  // return 0;
}

/* Unpacks the 32-bit machine code instruction given into the correct
 * type within the instruction struct */
Instruction parse_instruction(uint32_t instruction_bits) {
  /* YOUR CODE HERE */

  Instruction instruction;

  // add x8, x0, x0     hex : 00000433  binary = 0000 0000 0000 0000 0000 01000
  // Opcode: 0110011 (0x33) Get the Opcode by &ing 0x1111111, bottom 7 bits
  instruction.opcode = instruction_bits & ((1U << 7) - 1);

  // Shift right to move to pointer to interpret next fields in instruction.
  instruction_bits >>= 7;

  switch (instruction.opcode) {

  // R-Type
  case 0x33:
  case 0x0b:
    // instruction: 0000 0000 0000 0000 0000 destination : 01000
    instruction.rtype.rd = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>= 5;

    // instruction: 0000 0000 0000 0000 0 func3 : 000
    instruction.rtype.funct3 = instruction_bits & ((1U << 3) - 1);
    instruction_bits >>= 3;

    // instruction: 0000 0000 0000  src1: 00000
    instruction.rtype.rs1 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>= 5;

    // instruction: 0000 000        src2: 00000
    instruction.rtype.rs2 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>= 5;

    // funct7: 0000 000
    instruction.rtype.funct7 = instruction_bits & ((1U << 7) - 1);

    break;

  // I-type 
  case 0x03:
  case 0x13:
  case 0x73:
    
    // Fetching the rd bits
    instruction.itype.rd = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    // Fetching the funct3 bits
    instruction.itype.funct3 = instruction_bits & ((1U << 3) - 1);
    instruction_bits >>=3;

    // Fetching the rs1 bits
    instruction.itype.rs1 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;
    
    // Fetching the imm bits
    instruction.itype.imm = instruction_bits & ((1U << 12) - 1);
    
    break;

  // S-type:
  case 0x23:

    instruction.stype.imm5 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.stype.funct3 = instruction_bits & ((1U << 3) - 1);
    instruction_bits >>=3;

    instruction.stype.rs1 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.stype.rs2 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;
    
    instruction.stype.imm7 = instruction_bits & ((1U << 7) - 1);

    break;

  // B-Type
  case 0x63:

    instruction.sbtype.imm5 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.sbtype.funct3 = instruction_bits & ((1U << 3) - 1);
    instruction_bits >>=3;

    instruction.sbtype.rs1 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.sbtype.rs2 = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;
    
    instruction.sbtype.imm7 = instruction_bits & ((1U << 7) - 1);

    break;

  // U-Type
  case 0x37:
  case 0x17:

    instruction.utype.rd = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.utype.imm = instruction_bits & ((1U << 20) - 1);

    break;

  // J-Type:
  case 0x67:
  case 0x6F:

    instruction.ujtype.rd = instruction_bits & ((1U << 5) - 1);
    instruction_bits >>=5;

    instruction.ujtype.imm = instruction_bits & ((1U << 20) - 1);

    break;

  // Default Case:
  default:
    exit(EXIT_FAILURE);

  }

  return instruction;
}

// Logic I used during this portion: (x & 1 = x)  AND  (x & 0 = 0)  AND  (0 | x = x)

/* Return the number of bytes (from the current PC) to the branch label using
 * the given branch instruction */
int get_branch_offset(Instruction instruction) {
  /* YOUR CODE HERE */
  int offset = 0x0;
  // Last bit in imm7 is picked up (which corresponds to the [12] in offset):
  offset |= (instruction.sbtype.imm7 & 0b1000000) << 6;
  // First bit in imm5 is picked up (which corresponds to the [11] in offset):
  offset |= (instruction.sbtype.imm5 & 0b00001) << 11;
  // Bits in imm7 that correspond to [10:5] in offset are picked up:
  offset |= (instruction.sbtype.imm7 & 0b0111111) << 5;
  // Bits in imm 5 coresspond to [4:1] in offset are picked up. Here, I am taking 
  // the first five bits and making the last one zero as the first bit in this offset
  // is discarded: 
  offset |= instruction.sbtype.imm5 & 0b11110;
  return sign_extend_number(offset, 13);
}

/* Returns the number of bytes (from the current PC) to the jump label using the
 * given jump instruction */
int get_jump_offset(Instruction instruction) {
  /* YOUR CODE HERE */
  int offset = 0x0;
  // Last bit is picked up (which corresponds to the [20] in offset):
  offset |= (instruction.ujtype.imm & 0b10000000000000000000) << 1;
  // First 8 bits are picked up (which correspond to [19:12] in offset):
  offset |= (instruction.ujtype.imm & 0b00000000000011111111) << 12;
  // 9th bit is picked up (which corresponds to [11] in offset):
  offset |= (instruction.ujtype.imm & 0b00000000000100000000) << 3;
  // Bits that correspond to [10:1] in the offset are picked up:
  offset |= (instruction.ujtype.imm & 0b01111111111000000000) >> 8;
  return sign_extend_number(offset, 21);
}

int get_store_offset(Instruction instruction) {
  /* YOUR CODE HERE */
  int offset = 0x0;
  // The five bits in imm5 are picked up (which correspond to [4:0] in offset):
  offset |= instruction.stype.imm5 & 0b000000011111; 
  // The seven bits in imm7 are picked up (which correspond to [11:5] in offset):
  offset |= (instruction.stype.imm7 << 5) & 0b111111100000; 
  return sign_extend_number(offset, 12);
  // return 0;
}

void handle_invalid_instruction(Instruction instruction) {
  printf("Invalid Instruction: 0x%08x\n", instruction.bits);
}

void handle_invalid_read(Address address) {
  printf("Bad Read. Address: 0x%08x\n", address);
  exit(-1);
}

void handle_invalid_write(Address address) {
  printf("Bad Write. Address: 0x%08x\n", address);
  exit(-1);
}
