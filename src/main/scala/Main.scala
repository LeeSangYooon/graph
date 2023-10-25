import Interpreter.REPL

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    run(REPL.init)
  }
  @tailrec
  private def run(repl: REPL): REPL = run(repl.prompt())
}