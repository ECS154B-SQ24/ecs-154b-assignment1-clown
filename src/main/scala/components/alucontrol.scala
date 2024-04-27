// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  //Your code goes here

  // switch (io.funct3) {
  // is("b000".U) {
  //   io.operation := Mux(!io.funct7(5), Mux(io.aluop(0.U) === true.B, "b00001".U, "b00100".U), Mux(io.aluop(0.U) === true.B, "b00110".U, "b11111".U))
  // } // add - sub b00100 - mul b00110 - Invalid
  //   is ("b001".U) { io.operation := "b10010".U}//sll
  //   is ("b010".U) {io.operation := "b10110".U} //slt
  //   is ("b011".U) {io.operation := "b10111".U} //sltu
  //   is ("b100".U) {io.operation := "b01111".U} //xor
  //   is ("b101".U) {io.operation := Mux( !io.funct7(5),"b10100".U, "b10000".U)} //srl - sra
  //   is ("b110".U) {io.operation := "b01110".U} //or
  //   is ("b111".U) { io.operation := "b01101".U} //and
  // }

  switch(io.aluop) {
    is(0.U){
      io.operation := "b11111".U
    }
    is(1.U) { // 64 BIT R TYPE
      switch(io.funct3) {
        is("b000".U) { io.operation := Mux(!io.funct7(5), Mux(!io.funct7(0), "b00001".U, "b00110".U),"b00100".U)} // ADD - SUB - MUL
        is("b001".U) { io.operation := Mux(!io.funct7(0), "b10010".U, "b00111".U) } // SLL - MULH
        is("b010".U) { io.operation := Mux(!io.funct7(0), "b10110".U, "b11000".U) } // SLT - MULHSU
        is("b011".U) { io.operation := Mux(!io.funct7(0), "b10111".U, "b01000".U) } // SLTU - MULHU
        is("b100".U) { io.operation := Mux(!io.funct7(0), "b01111".U, "b01011".U) } // XOR - DIV
        is("b101".U) { io.operation := Mux(!io.funct7(0), Mux(!io.funct7(5), "b10100".U, "b10000".U), "b01010".U) } // SRL - SRA - DIVU
        is("b110".U) { io.operation := Mux(!io.funct7(0), "b01110".U, "b11100".U) } // OR - REM
        is("b111".U) { io.operation := Mux(!io.funct7(0), "b01101".U, "b11011".U) } // AND - REMU
      }
    }
    is(2.U) { //64 BIT I TYPE (IDK WHAT TO PUT IN HERE, IGNORE)
      switch(io.funct3) {
        is("b000".U) { io.operation := "b01100".U } // ADDI 
        is("b001".U) { io.operation := "b00110".U } // SLLI
        is("b010".U) { io.operation := "b01010".U } // SLTI
        is("b011".U) { io.operation := "b01011".U } // SLTUI
        is("b100".U) { io.operation := "b00010".U } // XORI
        is("b101".U) { io.operation := Mux(!io.funct7(0), "b01000".U, "b00100".U) } // SRLI - SRAI
        is("b110".U) { io.operation := "b00001".U } // ORI
        is("b111".U) { io.operation := "b00000".U } // ANDI
      }
    }
    is(3.U){ // 32 BIT R TYPE 
      switch(io.funct3){
        is("b000".U) { io.operation := Mux(!io.funct7(0), Mux(!io.funct7(5), "b00000".U, "b00010".U),"b00101".U)}    
        is("b001".U) { io.operation := "b10011".U } // SLLW
        is("b100".U) { io.operation := "b01001".U } // DIVW
        is("b101".U) { io.operation := Mux(!io.funct7(0), Mux(!io.funct7(5), "b10101".U, "b10001".U), "b01100".U) } // SRLW - SRAW - DIVUW
        is("b110".U) { io.operation := "b11010".U } // REMW
        is("b111".U) { io.operation := "b11001".U } // REMUW   
        }
    }
    is (4.U){ // 32 BIT I TYPE (IDK WHAT TO PUT IN HERE, IGNORE)
      switch(io.funct3){
        is("b000".U) { io.operation := "b01100".U } // ADDI 
        is("b001".U) { io.operation := "b00110".U } // SLLI
        is("b010".U) { io.operation := "b01010".U } // SLTI
        is("b011".U) { io.operation := "b01011".U } // SLTUI
        is("b100".U) { io.operation := "b00010".U } // XORI
        is("b101".U) { io.operation := Mux(!io.funct7(0), "b01000".U, "b00100".U) } // SRLI - SRAI
        is("b110".U) { io.operation := "b00001".U } // ORI
        is("b111".U) { io.operation := "b00000".U } // ANDI
      }
    }
    is (5.U){ //Non-Arthimetic instruction
      switch(io.funct3){
        is("b000".U){io.operation := Mux( !io.funct7(5), "b11101".U, "b11110".U)} // LOAD STORE IDK MADE UP
        is("b111".U){io.operation := "b111111".U}// INVALID
      }
    }
  }
}