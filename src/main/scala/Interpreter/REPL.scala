package Interpreter
import scala.io.StdIn.readLine
import Compiler.Compiler
import VirtualMachine.VirtualMachine
import Interpreter.VirtualMachine.Memory
import Interpreter.Code

class REPL(lines: List[String], virtualMachine: VirtualMachine, compiler: Compiler = new Compiler) {
  def prompt(): REPL = {
    //println(virtualMachine)
    print(">> ")
    val input = readLine()
    val code = compiler.compile(input)
    val newVirtualMachine = virtualMachine.run(code)
    val output = newVirtualMachine.memory.getOutput
    println(code)
    if (output != null) {
      output.show(newVirtualMachine)
    }
    new REPL(lines.appended(input), newVirtualMachine, compiler)
  }
}

object REPL{
  def init: REPL = {
    val vm = VirtualMachine(Memory())
    new REPL(List(), vm)
  }
}
