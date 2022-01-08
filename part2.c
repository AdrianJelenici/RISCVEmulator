#include <stdio.h> // for stderr
#include <stdlib.h> // for exit()
#include "types.h"
#include "utils.h"
#include "riscv.h"

void execute_rtype(Instruction, Processor *);
void execute_itype_except_load(Instruction, Processor *);
void execute_branch(Instruction, Processor *);
void execute_jal(Instruction, Processor *);
void execute_load(Instruction, Processor *, Byte *);
void execute_store(Instruction, Processor *, Byte *);
void execute_ecall(Processor *, Byte *);
void execute_lui(Instruction, Processor *);
void execute_mac(Instruction, Processor *);

void execute_instruction(uint32_t instruction_bits, Processor *processor,Byte *memory) {    
    Instruction instruction = parse_instruction(instruction_bits);
    switch(instruction.opcode) {
        case 0x33:
            execute_rtype(instruction, processor);
            break;
        case 0x13:
            execute_itype_except_load(instruction, processor);
            break;
        case 0x73:
            execute_ecall(processor, memory);
            break;
        case 0x63:
            execute_branch(instruction, processor);
            break;
        case 0x6F:
            execute_jal(instruction, processor);
            break;
        case 0x23:
            execute_store(instruction, processor, memory);
            break;
        case 0x03:
            execute_load(instruction, processor, memory);
            break;
        case 0x37:
            execute_lui(instruction, processor);
            break;
        case 0x0b:
            execute_mac(instruction, processor);
            break;
        default: // undefined opcode
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

// In all the below operations, I will be casting the registers into type sWord.
// Word because registers are 32 bit numbers, and sWord because they can be 
// holding negative values. When doing signed operations, we also want to be 
// using signed numbers. There are some exceptions to this case however (such as
// when we are not doing signed operations).  


void execute_mac(Instruction instruction, Processor *processor) {
  switch (instruction.sbtype.funct3) {
        case 0x0:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // Mac
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rd]) + 
                      ((sWord)processor->R[instruction.rtype.rs1]) *
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    processor->PC += 4;
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    exit(-1);
                break;      
            }
            break;
	      default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            exit(-1);
        break;
  }
}

void execute_rtype(Instruction instruction, Processor *processor) {
    switch (instruction.rtype.funct3){
        case 0x0:
            switch (instruction.rtype.funct7) {
                case 0x0:
                  // Add
                  processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) +
                      ((sWord)processor->R[instruction.rtype.rs2]);
                  processor->PC += 4;

                  break;
                case 0x1:
                  // Mul
                  processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) *
                      ((sWord)processor->R[instruction.rtype.rs2]);
                  processor->PC += 4;
                  break;
                case 0x20:
                    // Sub
                    processor->R[instruction.rtype.rd] =
                      ((sWord)processor->R[instruction.rtype.rs1]) -
                      ((sWord)processor->R[instruction.rtype.rs2]);
                    processor->PC += 4;
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    exit(-1);
                    break;
            }
            break;
        case 0x1:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // SLL
                    // I am not sign casting rs1 here because we don't want to shift in rs1's sign bit. It is thus better to leave it unsigned.
                     processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] << (sWord)processor->R[instruction.rtype.rs2];
                     processor->PC += 4;
                    break;
                case 0x1:
                    // MULH
 		    // Mulh stores the top 32 bits of the product (whereas mul stores the bottom 32 bits). I am thus shifting those bits in. 
                     processor->R[instruction.rtype.rd] = ((((sWord) processor->R[instruction.rtype.rs1]) * ((sWord) processor->R[instruction.rtype.rs2])) >> 16);
                     processor->PC += 4;
                     break;
            }
            break;
        case 0x2:
            // SLT
            processor->R[instruction.rtype.rd] = (sWord) processor->R[instruction.rtype.rs1] < (sWord)(sHalf) processor->R[instruction.rtype.rs2];
            processor->PC += 4;
            break;
        case 0x4:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // XOR
                    // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned. 
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] ^ processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                case 0x1:
                    // DIV
                    processor->R[instruction.rtype.rd] = (sWord) processor->R[instruction.rtype.rs1] / (sWord) processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    exit(-1);
                    break;
            }
            break;
        case 0x5:
            switch (instruction.rtype.funct7) {
                case 0x0:
                // SRL  
		// For srl we only want to shift in zeroes (not the registers's sign bit). It is thus better to leave rs1 unsigned.  
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] >> (sWord) processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                case 0x20:
                    // SRA
		    // For sra, we want to shift in the register's sign bit. We will thus type cast it. 
                    processor->R[instruction.rtype.rd] = (sWord) processor->R[instruction.rtype.rs1] >> (sWord) processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    exit(-1);
                break;
            }
            break;
        case 0x6:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // OR
                    // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned.
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] | processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                case 0x1:
                    // REM
                    processor->R[instruction.rtype.rd] = (sWord) processor->R[instruction.rtype.rs1] % (sWord) processor->R[instruction.rtype.rs2];
                    processor->PC += 4;
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    exit(-1);
                    break;
            }
            break;
        case 0x7:
            // AND
            // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned. 
            processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] & processor->R[instruction.rtype.rs2];
            processor->PC += 4;
            break;
        default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            exit(-1);
            break;
    }
}

void execute_itype_except_load(Instruction instruction, Processor *processor) {
    int shiftOp;
    switch (instruction.itype.funct3) {
        case 0x0:
            // ADDI
            // As instructed by the pdf document found in the debugging section on the assignment page, the immediate should be sign extended. 
            processor->R[instruction.itype.rd] = (sWord) processor->R[instruction.itype.rs1] + sign_extend_number(instruction.itype.imm,12);
            processor->PC += 4;
            break;
        case 0x1:
            // SLLI
             processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] << sign_extend_number(instruction.itype.imm,12);
             processor->PC += 4;
            break;
        case 0x2:
            // STLI
            // If rs1 is less than imm, 1 is stored in rd. Otherwise, 0 is stored. 
             processor->R[instruction.itype.rd] = ((sWord) processor->R[instruction.itype.rs1] < sign_extend_number(instruction.itype.imm,12))? 1 : 0;
             processor->PC += 4;
            break;
        case 0x4:
            // XORI
            // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned.
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] ^ sign_extend_number(instruction.itype.imm,12);
            processor->PC += 4;
            break;
        case 0x5:
            // Shift Right (You must handle both logical and arithmetic)
            // Here the same principles apply as srl and sra:
            shiftOp = instruction.itype.imm >> 10;
            switch(shiftOp) {
                case 0x0:
                    processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] >> sign_extend_number(instruction.itype.imm,12);
                    processor->PC += 4;
                    break;
                case 0x1:
                    processor->R[instruction.itype.rd] = (sWord) processor->R[instruction.itype.rs1] >> sign_extend_number(instruction.itype.imm,12);
                    processor->PC += 4;
                    break;
                default :
                    handle_invalid_instruction(instruction);
                    processor->PC += 4;
                    break;
            }
            break;
        case 0x6:
            // ORI
            // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned.
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] | sign_extend_number(instruction.itype.imm,12);
            processor->PC += 4;
            break;
        case 0x7:
            // ANDI
            // Bitwise operations should only be used with unsigned integer operands. It is thus better to leave them unsigned.
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] & sign_extend_number(instruction.itype.imm,12);
            processor->PC += 4;
            break;
        default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            break;
    }
}

void execute_ecall(Processor *p, Byte *memory) {
    Register i;
    
    // syscall number is given by a0 (x10)
    // argument is given by a1
    switch(p->R[10]) {
        case 1: // print an integer
            p->PC += 4;
            printf("%d",p->R[11]);
            break;
        case 4: // print a string
            for(i=p->R[11];i<MEMORY_SPACE && load(memory,i,LENGTH_BYTE);i++) {
                printf("%c",load(memory,i,LENGTH_BYTE));
                p->PC += 4;
            }
            break;
        case 10: // exit
            printf("exiting the simulator\n");
            p->PC += 4;
            exit(0);
            break;
        case 11: // print a character
            printf("%c",p->R[11]);
            p->PC += 4;
            break;
        default: // undefined ecall
            printf("Illegal ecall number %d\n", p->R[10]);
            p->PC += 4;
            exit(-1);
            break;
    }
}

void execute_branch(Instruction instruction, Processor *processor) {
    switch (instruction.sbtype.funct3) {
        case 0x0:
            // BEQ
            if (processor->R[instruction.sbtype.rs1] == processor->R[instruction.sbtype.rs2]) {
                // If they are equal, jumps to the offest
                processor->PC += get_branch_offset(instruction);
            } else {
                // If not, continues as normal
                processor->PC += 4;
            }
            break;
        case 0x1:
            // BNE
             if (processor->R[instruction.sbtype.rs1] != processor->R[instruction.sbtype.rs2]) {
                // If they are not equal, jumps to the offest
                processor->PC += get_branch_offset(instruction);
            } else {
                // If not, continues as normal
                processor->PC += 4;
            }
            break;
        default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            exit(-1);
            break;
    }
}


void execute_load(Instruction instruction, Processor *processor, Byte *memory) {
    switch (instruction.itype.funct3) {
        case 0x0:
            // LB
            processor->R[instruction.itype.rd] = load(memory, processor->R[instruction.itype.rs1] + instruction.itype.imm, LENGTH_BYTE);
            processor->R[instruction.itype.rd] = sign_extend_number(processor->R[instruction.itype.rd],8);
            processor->PC += 4;
            break;
        case 0x1:
            // LH
            processor->R[instruction.itype.rd] = load(memory, processor->R[instruction.itype.rs1] + instruction.itype.imm, LENGTH_HALF_WORD);
            processor->R[instruction.itype.rd] = sign_extend_number(processor->R[instruction.itype.rd],16);
            processor->PC += 4;
            break;
        case 0x2:
            // LW
            processor->R[instruction.itype.rd] = load(memory, processor->R[instruction.itype.rs1] + sign_extend_number(instruction.itype.imm,11), LENGTH_WORD);
            processor->PC += 4;
            break;
        default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            break;
    }
}

void execute_store(Instruction instruction, Processor *processor, Byte *memory) {
    switch (instruction.stype.funct3) {
        case 0x0:
            // SB
            store(memory, processor->R[instruction.stype.rs1] + get_store_offset(instruction), LENGTH_BYTE, processor->R[instruction.stype.rs2]);
            processor->PC += 4;
            break;
        case 0x1:
            // SH
            store(memory, processor->R[instruction.stype.rs1] + get_store_offset(instruction), LENGTH_HALF_WORD,
                      processor->R[instruction.stype.rs2]);
            processor->PC += 4;
            break;
        case 0x2:
            // SW
             store(memory, processor->R[instruction.stype.rs1] + get_store_offset(instruction), LENGTH_WORD, processor->R[instruction.stype.rs2]);
             processor->PC += 4;
            break;
        default:
            handle_invalid_instruction(instruction);
            processor->PC += 4;
            exit(-1);
            break;
    }
}

void execute_jal(Instruction instruction, Processor *processor) {
    /* YOUR CODE HERE */
    processor->R[instruction.ujtype.rd] = processor->PC + 4;
    processor->PC = processor->PC + ((Word) get_jump_offset(instruction));
}

void execute_lui(Instruction instruction, Processor *processor) {
    /* YOUR CODE HERE */
    processor->R[instruction.utype.rd] = (Word)instruction.utype.imm << 12;
    processor->PC += 4;
}

void store(Byte *memory, Address address, Alignment alignment, Word value) {
    /* YOUR CODE HERE */
    memory[address] = value;
    switch(alignment) {
    case 4:
      memory[address + 1] = value >> 8;
      memory[address + 2] = value >> 16;
      memory[address + 3] = value >> 24;
      break;
    case 2:
      memory[address + 1] = value >> 8;
      break;
    default:
      break;
    }
}

Word load(Byte *memory, Address address, Alignment alignment) {
    /* YOUR CODE HERE */
    Word loaded = 0;
    switch(alignment) {
    case 4:
      loaded = memory[address];
      loaded |= memory[address + 1] << 8;
      loaded |= memory[address + 2] << 16;
      loaded |= memory[address + 3] << 24;
      break;
    case 2:
      loaded = memory[address];
      loaded |= memory[address + 1] << 8;
      break;
    default:
       loaded = memory[address];
      break;
    }
      
   return loaded;
}
