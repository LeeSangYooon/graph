package Interpreter.VirtualMachine
import Interpreter.VirtualMachine.Memory
import Interpreter.{ByteCode, Output}
import Interpreter.Code

import scala.annotation.tailrec


case class VirtualMachine(memory: Memory){
  @tailrec
  private def runEach(code: Code, index: Int, stepMemory: Memory): Memory = {
    if (index >= code.bytecodes.length) {
      if (stepMemory.stack.isEmpty)
        return stepMemory
      else
        return stepMemory.output(Output(string = stepMemory.stack.last.toString))
    }
    val line = code.bytecodes(index)
    val next_memory = line match {
        case ByteCode.PLUS | ByteCode.MINUS | ByteCode.DIV | ByteCode.MULTI | ByteCode.POWER => {
          val a = stepMemory.stack.last
          val popped = stepMemory.popped
          val b = popped.stack.last
          popped.popped.pushed(opToFunc(line)(b, a))
        }
        case _ =>
          if (line.isPush) {
            stepMemory.pushed(line.parameter match {
              case d: Double => d
              case _ => throw new Exception("not double")
            })
          }
          else if (line.isAssign) {
            val name = line.parameter match {
              case s: String => s
              case _ => throw new Exception("not string")
            }
            val output = Output(f"Constant ${name} = ${stepMemory.stack.last}")
            stepMemory.popped.updated(Map(name -> stepMemory.stack.last)).output(output)
          }
          else if (line.isGet) {
            val name = line.parameter match {
              case s: String => s
              case _ => throw new Exception("not string")
            }
            val value = stepMemory.constants.get(name) match {
              case Some(d) => d
              case _ => throw new Exception(f"No variable named ${name}")
            }
            stepMemory.pushed(value)
          } else if (line.isCall) {
            val funcName = line.parameter match {
              case s: String => s
              case _ => throw new Exception("not string")
            }

            if ("sin" :: "cos" :: "tan" :: "ln" :: "abs" :: Nil contains funcName) {
              val last = stepMemory.stack.last
              val output = funcName match {
                case "sin" => Math.sin(last)
                case "cos" => Math.cos(last)
                case "tan" => Math.tan(last)
                case "ln" => Math.log(last)
                case "abs" => Math.abs(last)
              }
              stepMemory.copy(_stack = stepMemory.stack.dropRight(1).appended(output))
            }
            else {
              val func = stepMemory.functions(funcName)
              var new_machine = this.copy(memory = stepMemory)
              for (param <- func.params.reverse) {
                val varMap = new_machine.memory.constants ++ Map(param -> new_machine.memory.stack.last)
                val stack = new_machine.memory.stack.dropRight(1)
                new_machine = VirtualMachine(memory = new_machine.memory.copy(_stack = stack, _constants = varMap))
              }

              stepMemory.copy(_stack = new_machine.run(func.exp).memory.stack)
            }

          }
          else {
            throw new Exception("not implemented yet")
          }
    }

    runEach(code, index + 1, next_memory)
  }
  def run(code: Code): VirtualMachine = {
    val memoryWithEmptyStack = memory.copy(_stack = List.empty)
    val memory_updated = runEach(code, 0, memoryWithEmptyStack)

    if (code.functions.isEmpty) {
      VirtualMachine(memory_updated)
    } else {
      val func_updated_memory = memory_updated.func_added(code.functions)
      val added_func = code.functions.head._2
      val func_output_memory = func_updated_memory.output(Output(func = (added_func.name, added_func.params.length)))
      VirtualMachine(func_output_memory)
    }

  }

  private def opToFunc(op: ByteCode): (Double, Double) => Double = op match {
    case ByteCode.PLUS => _ + _
    case ByteCode.MINUS => _ - _
    case ByteCode.MULTI => _*_
    case ByteCode.DIV => _/_
    case ByteCode.POWER => Math.pow
  }
}
