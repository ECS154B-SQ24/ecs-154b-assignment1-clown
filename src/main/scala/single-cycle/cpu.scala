// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // Connecting inputs and outputs of unused modules to DontCare
  control.io         := DontCare
  registers.io       <> controlTransfer.io.registers
  aluControl.io      <> controlTransfer.io.aluControl
  alu.io             <> controlTransfer.io.alu
  immGen.io          <> controlTransfer.io.immGen
  controlTransfer.io.dmem := DontCare
  io.dmem            := DontCare

  // FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }

  // Extracting instruction fields
  val opcode = instruction(6,0)
  val rd = instruction(11,7)
  val funct3 = instruction(14,12)
  val rs1 = instruction(19,15)
  val rs2 = instruction(24,20)
  val funct7 = instruction(31,25)

  // Setting control signals based on opcode
  control.io.opcode := opcode

  // Reading data from register file
  registers.io.readreg1 := rs1
  registers.io.readreg2 := rs2
  registers.io.writereg := rd
  registers.io.wen := (registers.io.writereg =/= 0.U)
  registers.io.writedata := alu.io.result

  // Setting ALU control signals
  aluControl.io.aluop := control.io.aluop
  aluControl.io.funct3 := funct3
  aluControl.io.funct7 := funct7

  // Connecting ALU inputs and control signals
  alu.io.operation := aluControl.io.operation
  alu.io.operand1 := registers.io.readdata1
  alu.io.operand2 := registers.io.readdata2

  // Defining and connecting the next program counter module
  /*val nextpc = Module(new NextPCModule())
  nextpc.io.pc_or_x := pc
  pc := nextpc.io.nextpc*/

  val nextpc = controlTransfer.io.nextpc
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "controlTransfer"
    )
  }
}