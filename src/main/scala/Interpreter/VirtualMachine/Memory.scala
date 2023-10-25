package Interpreter.VirtualMachine

import Interpreter.Output
case class Memory(_stack: List[Double] = List(),
                  _constants: Map[String, Double] = Map("pi" -> 3.14159265358979311599796346854, "e" -> 2.718281828459045235360287471352),
                  _functions: Map[String, Func] = Map(), _output: Output = null) {
  val stack: List[Double] = _stack
  val constants: Map[String, Double] = _constants
  val functions: Map[String, Func] = _functions

  def popped: Memory = new Memory(stack.dropRight(1), constants, functions, _output)
  def pushed(item: Double): Memory = new Memory(stack.appended(item), constants, functions, _output)
  def updated(map: Map[String, Double]): Memory = new Memory(stack, _constants ++ map, functions, _output)
  def func_added(funcMap: Map[String, Func]): Memory = this.copy(_functions = this.functions ++ funcMap)
  def get(key: String): Option[Double] = constants.get(key)
  def output(o: Output) = new Memory(stack, _constants, functions, o)
  def getOutput: Output = _output
}
