package Interpreter.Compiler

case class Token(number: Int, char: String, isValue: Boolean = false, value: Double = 0,
                 isIdentifier: Boolean = false, name: String = "") {
  override def toString: String = {
    if (isValue) {
      value.toString
    } else if(isIdentifier) {
      name
    } else {
      char
    }
  }
}
object Tokens {
  val LEFT_PARENTHESES: Token = Token(0, "(")
  val RIGHT_PARENTHESES: Token = Token(1, ")")
  val LEFT_BRACE: Token = Token(3, "{")
  val RIGHT_BRACE: Token = Token(4, "}")
  val ASSIGNMENT: Token = Token(5, "=")
  val IF: Token = Token(6, "if")
  val COMMA: Token = Token(7, ",")
  val PLUS: Token = Token(8, "+")
  val MINUS: Token = Token(9, "-")
  val MULTI: Token = Token(10, "*")
  val DIV: Token = Token(11, "/")
  val EQUAL: Token = Token(12, "==")
  val GREATER: Token = Token(13, ">")
  val LESSER: Token = Token(14, "<")
  val POW: Token = Token(15, "^")
  val FUNC: Token = Token(16, "func") // Not Identifier
  val ELIF: Token = Token(17, "elif")
  val ELSE: Token = Token(18, "else")
  val WHITESPACE: Token = Token(19, " ")
  val NEWLINE: Token = Token(20, "\n")
}
