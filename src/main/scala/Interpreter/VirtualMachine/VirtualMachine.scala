package Interpreter.VirtualMachine
import Interpreter.VirtualMachine.Memory
import Interpreter.{ByteCode, Output}
import Interpreter.Code


case class VirtualMachine(memory_init: Memory, show:Boolean=false){
  val memory: Memory = memory_init
  def run(code: Code): VirtualMachine = {
    val lines = code.bytecodes
    var stepMemory = memory.copy()
    var next = 0
    for (i <- lines.indices; line = lines(i) if i >= next) {
      stepMemory =
        line match {
          case ByteCode.PLUS | ByteCode.MINUS | ByteCode.DIV | ByteCode.MULTI | ByteCode.POWER |
               ByteCode.GREATER | ByteCode.LESSER | ByteCode.EQUAL | ByteCode.GREATER_OR_EQUAL | ByteCode.LESSER_OR_EQUAL |
               ByteCode.AND | ByteCode.OR => {
            val a = stepMemory.stack.last
            val popped = stepMemory.popped
            val b = popped.stack.last
            popped.popped.pushed(opToFunc(line)(b,a))
          }
          case ByteCode.POP =>
            stepMemory.popped
          case ByteCode.IF =>{
            if (stepMemory.stack.last != 1)
              next = i + 2
            stepMemory
          }
          case _ => {
            if (line.isPush){
              stepMemory.pushed(line.parameter match {
                case d: Double => d
                case _ => throw new Exception("not double")
              })
            }
            else if(line.isAssign) {
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
            }
            else if (line.isSkip) {
              val n = line.parameter match {
                case num: Int => num
                case _ => throw new Exception("not int")
              }
              next = i + n + 1 // next
              stepMemory
            }

            else if (line.isCall) {
              val funcName = line.parameter match {
                case s: String => s
                case _ => throw new Exception("not string")
              }

              if ("sin"::"cos"::"tan"::"ln"::"abs"::Nil contains funcName) {
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
                var new_machine = this.copy(memory_init = stepMemory, show=false)
                for (param <- func.params.reverse) {
                  val varMap = new_machine.memory.constants ++ Map(param -> new_machine.memory.stack.last)
                  val stack = new_machine.memory.stack.dropRight(1)
                  new_machine = VirtualMachine(memory_init = new_machine.memory.copy(_stack = stack, _constants = varMap))
                }

                stepMemory.copy(_stack = new_machine.run(func.exp).memory.stack)
              }

            }
            else {
              throw new Exception("not implemented yet")
            }
          }
        }
    }
    if (code.functions.isEmpty) {
      if (stepMemory.stack.isEmpty || !show)
        VirtualMachine(stepMemory)
      else
        VirtualMachine(stepMemory.output(Output(stepMemory.stack.last.toString)))
    } else {
      val func_updated_memory = stepMemory.func_added(code.functions)
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

    case ByteCode.EQUAL =>
      (a, b) => if (a == b) 1 else 0

    case ByteCode.GREATER =>
      (a, b) => if (a > b) 1 else 0

    case ByteCode.LESSER =>
      (a, b) => if (a < b) 1 else 0

    case ByteCode.GREATER_OR_EQUAL =>
      (a, b) => if (a >= b) 1 else 0

    case ByteCode.LESSER_OR_EQUAL =>
      (a, b) => if (a <= b) 1 else 0

    // 1인 값만 참이고 나머진 거짓
    case ByteCode.AND =>
      (a, b) => if (a == 1 && b == 1) 1 else 0

    case ByteCode.OR =>
      (a, b) => if (a == 1 || b == 1) 1 else 0


  }
}