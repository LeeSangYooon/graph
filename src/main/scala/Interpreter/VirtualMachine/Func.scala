package Interpreter.VirtualMachine

import Interpreter.Code
import Interpreter.Compiler.ExpressionAST

case class Func(name: String, params: List[String], exp: Code)
