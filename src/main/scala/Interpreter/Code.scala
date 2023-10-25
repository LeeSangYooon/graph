package Interpreter
import scala.collection.immutable.Map
import VirtualMachine.Func
case class Code(bytecodes: List[ByteCode] = List.empty, functions: Map[String, Func] = Map.empty) {
  def +(other: Code): Code = Code(bytecodes ::: other.bytecodes, functions ++ other.functions)
  def +(bytecode: ByteCode): Code = Code(bytecodes appended bytecode, functions)
}
object Code{
  def empty = new Code(List())
}
