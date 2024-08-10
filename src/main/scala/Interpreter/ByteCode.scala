package Interpreter

import Interpreter.VirtualMachine.Func

case class ByteCode (op: Byte, parameter: Any = null) {
  override def toString: String = {
    val name =
      op match {
        case 0 => "PUSH"
        case 1 => "PLUS"
        case 2 => "MINUS"
        case 3 => "MULTI"
        case 4 => "DIV"
        case 5 => "POWER"
        case 6 => "ASSIGN"
        case 7 => "GET"
        case 8 => "CALL"
        case 9 => "EQUAL"
        case 10 => "GREATER"
        case 11 => "LESSER"
        case 12 => "AND"
        case 13 => "OR"
        case 14 => "GREATER_OR_EQUAL"
        case 15 => "LESSER_OR_EQUAL"
        case 16 => "SKIP"
        case 17 => "POP"
        case 18 => "IF"
      }
    val p = parameter match {
      case d: Double => d.toString
      case s: String => s
      case f: Func => f.name
      case n: Int => n
      case _ => ""
    }
    name + " " + p
  }
  def isAssign: Boolean = op == 6
  def isPush: Boolean = op == 0
  def isGet: Boolean = op == 7
  def isCall: Boolean = op == 8
  def isSkip: Boolean = op == 16
}

object ByteCode {
  def PUSH(d: Double): ByteCode = new ByteCode(0, d)
  val PLUS: ByteCode = new ByteCode(1)
  val MINUS: ByteCode = new ByteCode(2)
  val MULTI: ByteCode = new ByteCode(3)
  val DIV: ByteCode = new ByteCode(4)
  val POWER: ByteCode = new ByteCode(5)
  val ASSIGN: String => ByteCode = name => new ByteCode(6, name)
  def GET(name: String): ByteCode = new ByteCode(7, name)
  def Call(func: String): ByteCode = new ByteCode(8, func)
  val EQUAL: ByteCode = new ByteCode(op = 9)
  val GREATER: ByteCode = new ByteCode(op = 10)
  val LESSER: ByteCode = new ByteCode(op = 11)
  val AND: ByteCode = new ByteCode(op = 12)
  val OR: ByteCode = new ByteCode(op = 13)
  val GREATER_OR_EQUAL: ByteCode = new ByteCode(op = 14)
  val LESSER_OR_EQUAL: ByteCode = new ByteCode(op = 15)
  def SKIP(n: Int): ByteCode = new ByteCode(op = 16, n)
  val POP: ByteCode = new ByteCode(op = 17)
  val IF: ByteCode = new ByteCode(op = 18)

}
