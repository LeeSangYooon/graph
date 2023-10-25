package Interpreter
import scala.io.StdIn.readLine
import Compiler.Compiler
import VirtualMachine.VirtualMachine
import Interpreter.VirtualMachine.Memory
import Interpreter.Code

class REPL(lines: List[String], virtualMachine: VirtualMachine, compiler: Compiler = new Compiler) {
  def prompt(): REPL = {
    //println(virtualMachine)
    if (lines.isEmpty) {
      println(Console.BLUE + "type #help to see commands" + Console.BLACK)
    }
    print(">> ")
    val input = readLine()
    val code = compiler.compile(input)
    val newVirtualMachine = virtualMachine.run(code)
    val output = newVirtualMachine.memory.getOutput
    println(code)
    if (output != null) {
      output.show(newVirtualMachine)
    }
    val stackEmpty = newVirtualMachine.copy(memory_init = newVirtualMachine.memory.copy(_stack = List.empty), show=true)
    new REPL(lines.appended(input), stackEmpty, compiler)
  }
}

object REPL{
  def init: REPL = {
    val vm = VirtualMachine(Memory(), show=true)
    new REPL(List(), vm)
  }
}
