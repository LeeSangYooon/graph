package Interpreter.Compiler
import Interpreter.{ByteCode, Code}
import Interpreter.Compiler.AST
import Interpreter.VirtualMachine.Func
import Interpreter.Code

import collection.mutable
object ByteCodeGenerator {
  private def tokenOpToByte(op: Token) = op match {
    case Tokens.PLUS => ByteCode.PLUS
    case Tokens.MINUS => ByteCode.MINUS
    case Tokens.MULTI => ByteCode.MULTI
    case Tokens.DIV => ByteCode.DIV
    case Tokens.POW => ByteCode.POWER
    case Tokens.EQUAL => ByteCode.EQUAL
    case Tokens.GREATER => ByteCode.GREATER
    case Tokens.LESSER => ByteCode.LESSER
    case Tokens.AND => ByteCode.AND
    case Tokens.OR => ByteCode.OR
    case Tokens.GREATER_OR_EQUAL => ByteCode.GREATER_OR_EQUAL
    case Tokens.LESSER_OR_EQUAL => ByteCode.LESSER_OR_EQUAL
  }
  private def generateExp(exp: ExpressionAST): Code = {
    exp match {
      case b: BinaryOperationAST => generateExp(b.left) + generateExp(b.right) + Code(List(tokenOpToByte(b.op)))
      case a: AtomAST =>
        a match {
          case v: ValueAST => Code(List(ByteCode.PUSH(v.value)))
          case v: VariableAST =>  {
            Code(List(ByteCode.GET(v.name)))
          }
          case FuncCallAST(funcName, params) => {
            val param_expressions = params.map(p => generateExp(p)).foldLeft(Code.empty)(_+_)
            val call_expression = Code(bytecodes = List(ByteCode.Call(funcName)))
            param_expressions + call_expression
          }
        }
      case i: IfElseAST => {
        // i.cond
        // if
        // skip i.ifElse.length + 2 (맞으면 밑에 세개 스킵)
        // pop
        // i.ifElse
        // skip ifThen.length + 1 (else 돈 놈이 밑에꺼 두개)
        // pop
        // i.ifThen
        println(i)
        val firstSkipLength = generateExp(i.ifElse).bytecodes.length + 2 // if true
        val secondSkipLength = generateExp(i.ifThen).bytecodes.length + 1 // if false
        generateExp(i.condition) +
          Code(List(ByteCode.IF)) +
          Code(List(ByteCode.SKIP(firstSkipLength))) +
          Code(List(ByteCode.POP)) +
          generateExp(i.ifElse) +
          Code(List(ByteCode.SKIP(secondSkipLength))) +
          Code(List(ByteCode.POP)) +
          generateExp(i.ifThen)
      }

    }
  }

  def generateByteCode(ast: RootAST, functions: mutable.Map[String, Func], variables: mutable.Set[String]): Code = {
    ast.statements.map {
      case exp: ExpressionAST => generateExp(exp)
      case t: FuncDeclAST => {
        val func_exp = generateExp(t.exp)
        val func = Func(t.func_name, t.params, func_exp)
        Code(List.empty, Map(func.name -> func))
      }
      case v: VarAssignmentAST => {
        if (variables contains v._variable)
          throw new Exception(f"Constant '${v.variable}' has been already defined")
        if (functions.keys.toList contains v.variable)
          throw new Exception(f"Function '${v.variable}' has been defined")
        variables += v.variable
        generateExp(v._expressionAST) + Code(List(ByteCode.ASSIGN(v._variable)))
      }
      case _ => Code.empty
    }.foldLeft(Code(List.empty))(_+_)
  }
}
