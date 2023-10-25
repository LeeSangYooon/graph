package Interpreter.Compiler
import Interpreter.ByteCode
import Interpreter.Compiler.ByteCodeGenerator.generateByteCode
import Interpreter.Compiler.Tokenizer.tokenize
import Interpreter.VirtualMachine.Func
import Interpreter.Code
import scala.collection.mutable


class Compiler {
  private def show(code: Code): Unit = println(code.bytecodes.mkString("\n"), code.functions)
  private var functions: mutable.Map[String, Func] = mutable.Map()
  private var variables: mutable.Set[String] = mutable.Set()

  def compile(string: String): Code =  {
    val tokens = tokenize(string)
    val ast = RootAST.generate(tokens)
    println(ast)
    val code = generateByteCode(ast, functions, variables)
    show(code)
    code
  }



}
