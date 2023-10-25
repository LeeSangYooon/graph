package Interpreter.Compiler
import Interpreter.ByteCode
import Interpreter.Compiler.ByteCodeGenerator.generateByteCode
import Interpreter.Compiler.Tokenizer.tokenize
import Interpreter.VirtualMachine.Func
import Interpreter.Code
import scala.collection.mutable


class Compiler {
  private def show(code: Code): Unit = println(code.bytecodes.mkString("\n"), code.functions)

  def compile(string: String): Code =  {
    val tokens = tokenize(string)
    val ast = RootAST.generate(tokens)
    val code = generateByteCode(ast, mutable.Map(), mutable.Set())
    show(code)
    code
  }
}
