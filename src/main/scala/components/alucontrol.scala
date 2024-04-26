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
  //First when checks whether or not aluop is 1
  when(io.aluop === "b1".U){
    when(io.funct3 === "b000".U){
      when(!io.funct7(5) === "b0".U){
        io.operation := "b00100".U
      }
      when(!io.funct7(5) === "b1".U){
        io.operation := Mux(!io.funct7(0), "b00001".U, "b00110".U)
      }
    }
    when(io.funct3 === "b001".U){
      io.operation := Mux(!io.funct7(0),"b10010".U,"b00111".U)
    }
    when(io.funct3 === "b010".U){
      io.operation := Mux(!io.funct7(0),"b10110".U,"b11000".U)
    }
    when(io.funct3 === "b011".U){
      io.operation := Mux(!io.funct7(0),"b10111".U,"b01000".U)
    }
    when(io.funct3 === "b100".U){
      io.operation := Mux(!io.funct7(0),"b01111".U,"b01011".U)
    }
    when(io.funct3 === "b101".U){
      when(!io.funct7(0) === "b0".U){
        io.operation := "b01010".U
      }
      when(!io.funct7(0) === "b1".U){
        io.operation := Mux(!io.funct7(5),"b10100".U,"b10000".U)
      }
    }
    when(io.funct3 === "b110".U){
      io.operation := Mux(!io.funct7(0),"b01110".U,"b11100".U)
    }
    when(io.funct3 === "b111".U){
      io.operation := Mux(!io.funct7(0),"b01101".U,"b11011".U)
    }
  }
  //First when checks whether or not aluop is 3
  when(io.aluop === "b11".U){
    when(io.funct3 === "b000".U){
      when(!io.funct7(5) === "b0".U){
        io.operation := "b00010".U
      }
      when(!io.funct7(5) === "b1".U){
        io.operation := Mux(!io.funct7(0), "b00000".U, "b00101".U)
      }
    }
    when(io.funct3 === "b001".U){
      io.operation := "b10011".U
    }
    when(io.funct3 === "b101".U){
      when(!io.funct7(5) === "b0".U){
        io.operation := "b10001".U
      }
      when(!io.funct7(5) === "b1".U){
        io.operation := Mux(!io.funct7(0), "b10101".U, "b01100".U)
      }
    }
    when(io.funct3 === "b100".U){
      io.operation := "b01001".U
    }
    when(io.funct3 === "b100".U){
      io.operation := "b01001".U
    }
    when(io.funct3 === "b110".U){
      io.operation := "b11010".U
    }
    when(io.funct3 === "b111".U){
      io.operation := "b11001".U
    }
  }
}