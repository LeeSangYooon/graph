package Interpreter.VirtualMachine
import Interpreter.VirtualMachine.Memory
import Interpreter.{ByteCode, Output}
import Interpreter.Code


case class VirtualMachine(memory_init: Memory){
  var memory: Memory = memory_init
  def run(code: Code): VirtualMachine = {
    val lines = code.bytecodes
    for (line <- lines) {
      memory =
        line match {
          case ByteCode.PLUS | ByteCode.MINUS | ByteCode.DIV | ByteCode.MULTI | ByteCode.POWER => {
            val a = memory.stack.last
            val popped = memory.popped
            val b = popped.stack.last
            popped.popped.pushed(opToFunc(line)(b,a))
          }
          case _ => {
            if (line.isPush){
              memory.pushed(line.parameter match {
                case d: Double => d
                case _ => throw new Exception("not double")
              })
            }
            else if(line.isAssign) {
              val name = line.parameter match {
                case s: String => s
                case _ => throw new Exception("not string")
              }
              val output = Output(f"Constant ${name} = ${memory.stack.last}")
              memory.popped.updated(Map(name -> memory.stack.last)).output(output)
            }
            else if (line.isGet) {
              val name = line.parameter match {
                case s: String => s
                case _ => throw new Exception("not string")
              }
              val value = memory.constants.get(name) match {
                case Some(d) => d
                case _ => throw new Exception(f"No variable named ${name}")
              }
              memory.pushed(value)
            } else if (line.isCall) {
              val funcName = line.parameter match {
                case s: String => s
                case _ => throw new Exception("not string")
              }

              if ("sin"::"cos"::"tan"::"ln"::"abs"::Nil contains funcName) {
                val last = memory.stack.last
                val output = funcName match {
                  case "sin" => Math.sin(last)
                  case "cos" => Math.cos(last)
                  case "tan" => Math.tan(last)
                  case "ln" => Math.log(last)
                  case "abs" => Math.abs(last)
                }
                memory.copy(_stack = memory.stack.dropRight(1).appended(output))
              }
              else {
                val func = memory.functions(funcName)
                var new_machine = this.copy(memory_init = memory)
                for (param <- func.params.reverse) {
                  val varMap = new_machine.memory.constants ++ Map(param -> new_machine.memory.stack.last)
                  val stack = new_machine.memory.stack.dropRight(1)
                  new_machine = VirtualMachine(memory_init = new_machine.memory.copy(_stack = stack, _constants = varMap))
                }

                memory.copy(_stack = new_machine.run(func.exp).memory.stack)
              }

            }
            else {
              throw new Exception("not implemented yet")
            }
          }
        }
    }
    if (code.functions.isEmpty) {
      VirtualMachine(memory)
    } else {
      val func_updated_memory = memory.func_added(code.functions)
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