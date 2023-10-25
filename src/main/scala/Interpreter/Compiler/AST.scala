package Interpreter.Compiler

import scala.annotation.tailrec
import scala.collection.mutable

object Util {
  def addPadding(string: String): String =
    string.split('\n').map(s => "\n.."+s).foldLeft("")(_+_)

  def readParentheses(tokens: List[Token]): (List[List[Token]], Int) = {
    //println(tokens)
    assert(tokens.head == Tokens.LEFT_PARENTHESES)
    var i = 1
    var array = Array[List[Token]]()
    var stop = false
    while (i < tokens.length && !stop) {
      tokens(i) match {
        case Tokens.COMMA => throw new Exception()
        case _ => {
          var j = i
          var opens = 0
          while (!(opens == 0 && tokens(j) == Tokens.COMMA) && !(opens == 0 && tokens(j) == Tokens.RIGHT_PARENTHESES)) {
            assert(opens >= 0)
            val delta = tokens(j) match {
              case Tokens.LEFT_BRACE => 1
              case Tokens.LEFT_PARENTHESES => 1
              case Tokens.RIGHT_BRACE => -1
              case Tokens.RIGHT_PARENTHESES => -1
              case _ => 0
            }
            opens += delta
            j += 1
          }
          val exp = tokens.slice(i, j)
          array = array.appended(exp)
          i = j

          if (opens == 0 && tokens(j) == Tokens.RIGHT_PARENTHESES)
            stop = true
        }
      }
      i += 1
    }
    (array.toList, i)
  }

}

class AST {

}

case class RootAST(statements: List[StatementAST]) extends AST

object RootAST {
  def generate(tokens: List[Token]): RootAST = {
    val indexes = indexStatements(tokens, List(0), 0, 0)
    val statements = (for (i <- 1 until indexes.length)
      yield StatementAST.generate(tokens.slice(indexes(i - 1), indexes(i)))
      ).toList
    new RootAST(statements)
  }

  @tailrec
  private def indexStatements(tokens: List[Token], indexes: List[Int], opens: Int, i: Int): List[Int] = {
    if (tokens.isEmpty) indexes.appended(i)
    else
      tokens.head match {
        case Tokens.LEFT_BRACE => indexStatements(tokens.drop(1), indexes, opens + 1, i + 1)
        case Tokens.LEFT_PARENTHESES => indexStatements(tokens.drop(1), indexes, opens + 1, i + 1)
        case Tokens.RIGHT_BRACE => indexStatements(tokens.drop(1), indexes, opens - 1, i + 1)
        case Tokens.RIGHT_PARENTHESES => indexStatements(tokens.drop(1), indexes, opens - 1, i + 1)
        case Tokens.NEWLINE => {
          if (opens == 0)
            indexStatements(tokens.drop(1), indexes.appended(i), 0, i + 1)
          else
            indexStatements(tokens.drop(1), indexes, 0, i)
        }
        case _ => indexStatements(tokens.drop(1), indexes, opens, i + 1)
      }
  }
}

class StatementAST() extends AST
object StatementAST {
  def generate(tokens_init: List[Token]): StatementAST = {
    assert(!tokens_init.contains(Tokens.WHITESPACE))
    val tokens = tokens_init.filterNot(t => t == Tokens.NEWLINE)
    if (tokens.head.isIdentifier) {
      if (tokens.length == 1){
        return ExpressionAST.generate(tokens)
      }
      tokens(1) match {
        case Tokens.ASSIGNMENT =>
          val expressionAST = ExpressionAST.generate(tokens.drop(2))
          VarAssignmentAST(tokens.head.name, expressionAST)
        case Tokens.LEFT_PARENTHESES =>
          //println(tokens.drop(1))
          val (items, i) = Util.readParentheses(tokens.drop(1))
          val pointer = i + 1
          if (tokens.length == pointer) {
            ExpressionAST.generate(tokens)
          } else {
            tokens(pointer) match {
              case Tokens.ASSIGNMENT => {
                assert(items.forall(a => a.length == 1))
                val nameList = items.map(a => a.head.name)
                val exp =
                  ExpressionAST.generate(
                    if (tokens(pointer + 1) == Tokens.LEFT_BRACE)
                      readBracket(tokens.drop(pointer + 1))
                    else
                      tokens.drop(pointer + 1))
                // 함수 선언
                new FuncDeclAST(tokens.head.name, nameList, exp)
              }
              case _ => ExpressionAST.generate(tokens)
            }
          }
        case _ =>
          ExpressionAST.generate(tokens)
      }
    }
    else {
      ExpressionAST.generate(tokens)
    }
  }

  private def readBracket(tokens: List[Token]): List[Token] = ???
}

case class VarAssignmentAST(_variable: String, _expressionAST: ExpressionAST) extends StatementAST {
  val variable: String = _variable
  val expressionAST: ExpressionAST = _expressionAST
}

case class FuncDeclAST(func_name:String, params: List[String], exp: ExpressionAST) extends StatementAST


// 2 + 3 * 4 * 6 + 5 = 2 + (3 * 4) + 5 = 2 + 12 + 5 = 19
// (2 - 3 + 3) * 4
// 2 72 5
// + + +
// 1 + 2 * 3 ^ 4
abstract class ExpressionAST() extends StatementAST

object ExpressionAST {
  def generate(tokens: List[Token]): ExpressionAST = {
    var i = 0
    val expStack = mutable.Stack[ExpressionAST]()
    val opStack = mutable.Stack[Token]()

    val level = (t: Token) => t match {
      case Tokens.PLUS | Tokens.MINUS => 0
      case Tokens.MULTI | Tokens.DIV => 1
      case Tokens.POW => 2
      case _ => throw new Exception("wrong operator")
    }

    val isOp = (token: Token) => token == Tokens.PLUS || token == Tokens.MINUS ||
      token == Tokens.MULTI || token == Tokens.DIV || token == Tokens.POW

    while (i < tokens.length) {
      val token = tokens(i)
      if (isOp(token)) {
        while (opStack.nonEmpty && level(opStack.top) >= level(token)) {
          val (right, left) = (expStack.pop(), expStack.pop())
          val op = opStack.pop()
          val out = new BinaryOperationAST(left, right, op)
          expStack.push(out)
        }
        opStack.push(token)
      }
      else if (token == Tokens.LEFT_PARENTHESES) {
        val (items, k) = Util.readParentheses(tokens.drop(i))
        //println(items, k, tokens)
        assert(items.length == 1)
        val exp = ExpressionAST.generate(items.head)
        expStack.push(exp)
        i += k -1
      }
      else if (token.isValue) {
        expStack.push(new ValueAST(token.value))
      }
      else if (token.isIdentifier) {
        if (tokens.length > i + 1 && tokens(i+1) == Tokens.LEFT_PARENTHESES) {
          val (items, k) = Util.readParentheses(tokens.drop(i + 1))
          val expList = items.map(item => ExpressionAST.generate(item))
          expStack.push(new FuncCallAST(token.name, expList))
          i += k
        } else {
          expStack.push(new VariableAST(_variableName = token.name))
        }
      }
      else {
        //println(token)
        throw new Exception("")
      }
      // 괄호 추가
      i += 1
    }

    //println(expStack)
    //println(opStack)
    while (expStack.length >= 2) {
      val right = expStack.pop()
      val left = expStack.pop()
      val op = opStack.pop()
      val exp = new BinaryOperationAST(left, right, op)
      expStack.push(exp)
    }
    assert(opStack.isEmpty)
    val root =expStack.pop()
    root
  }
}

class BinaryOperationAST(_left: ExpressionAST, _right: ExpressionAST, _op: Token) extends ExpressionAST {
  val left: ExpressionAST = _left
  val right: ExpressionAST = _right
  val op: Token = _op
  override def toString: String = f"(${op.toString} ${left}, ${right})"   /*op.toString + Util.addPadding(left.toString) + Util.addPadding(right.toString)*/
}
class AtomAST() extends ExpressionAST {
}
class ValueAST(_value: Double) extends AtomAST {
  val value: Double = _value
  override def toString: String = "value: " + value.toString
}
class VariableAST(_variableName: String) extends AtomAST {
  val name: String = _variableName

  override def toString: String = f"variable ${name}"
}
case class FuncCallAST(funcName: String, params: List[ExpressionAST]) extends AtomAST {
  override def toString: String = f"function call: ${funcName} ${params.map(p => p.toString).foldLeft("(")(_+_+", ").dropRight(2)+ ")"}"
}