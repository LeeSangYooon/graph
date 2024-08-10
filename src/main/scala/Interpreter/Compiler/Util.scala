package Interpreter.Compiler

object Util {
  def addPadding(string: String): String =
    string.split('\n').map(s => "\n.."+s).foldLeft("")(_+_)

  // get tokens starting with (, returns list of list of tokens and index right after )
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

  def readBraces(tokens: List[Token]): (List[List[Token]], Int) = {
    assert(tokens.head == Tokens.LEFT_BRACE)
    var i = 1
    var array = Array[List[Token]]()
    var stop = false
    while (i < tokens.length && !stop) {
      tokens(i) match {
        case Tokens.COMMA => throw new Exception()
        case _ => {
          var j = i
          var opens = 0
          while (!(opens == 0 && tokens(j) == Tokens.COMMA) && !(opens == 0 && tokens(j) == Tokens.RIGHT_BRACE)) {
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

          if (opens == 0 && tokens(j) == Tokens.RIGHT_BRACE)
            stop = true
        }
      }
      i += 1
    }
    (array.toList, i)
  }

}
