package Interpreter.Compiler

object Tokenizer {
  def tokenize(string: String): List[Token] = {
    var tokens: List[Token] = List()
    var i = 0
    while (i < string.length) {
      val c = string(i)
      val dict = Map('(' -> Tokens.LEFT_PARENTHESES, ')' -> Tokens.RIGHT_PARENTHESES,
        '{' -> Tokens.LEFT_BRACE, '}' -> Tokens.RIGHT_BRACE,
        '+' -> Tokens.PLUS, '-' -> Tokens.MINUS, '*' -> Tokens.MULTI, '/' -> Tokens.DIV, '^' -> Tokens.POW,
        ',' -> Tokens.COMMA,
        ' ' -> Tokens.WHITESPACE, '\n' -> Tokens.NEWLINE
      )
      dict.get(c) match {
        case Some(value) => tokens = tokens.appended(value)
        case None => {
          if (c == '=') {
            if (string(i + 1) == '=') { // ==
              i += 1
              tokens = tokens.appended(Tokens.EQUAL)
            } else { // =
              tokens = tokens.appended(Tokens.ASSIGNMENT)
            }

          }
          else if (c == '>') {
            if (string(i + 1) == '=') { // >=
              i += 1
              tokens = tokens.appended(Tokens.GREATER_OR_EQUAL)
            } else { // >
              tokens = tokens.appended(Tokens.GREATER)
            }
          }
          else if (c == '<') {
            if (string(i + 1) == '=') { // <=
              i += 1
              tokens = tokens.appended(Tokens.LESSER_OR_EQUAL)
            } else { // <
              tokens = tokens.appended(Tokens.LESSER)
            }
          }
          /*else if (string.length >= i + 4 && string.slice(i, i + 4) == "func") {
            tokens = tokens.appended(Tokens.FUNC)
            i += 3
          }*/
          else if (string.length >= i + 3 && string.slice(i, i + 3) == "and") {
            tokens = tokens.appended(Tokens.AND)
            i += 2
          }
          else if (string.length >= i + 2 && string.slice(i, i + 2) == "or") {
            tokens = tokens.appended(Tokens.OR)
            i += 1
          }
          else if (string.length >= i + 2 && string.slice(i, i + 2) == "if") {
            tokens = tokens.appended(Tokens.IF)
            i += 1
          } else if (string.length >= i+4 && (string.slice(i, i + 4) == "elif" || string.slice(i, i + 4) == "else")) {
            if (string.slice(i, i + 4) == "elif") {
              tokens = tokens.appended(Tokens.ELIF)
              i += 3
            } else if (string.slice(i, i + 4) == "else") {
              tokens = tokens.appended(Tokens.ELSE)
              i += 3
            }
          } else {
            var s = ""
            while (i < string.length && (string(i).isLetterOrDigit || string(i) == '_' || string(i) == '.')) {
              s = s.appended(string(i))
              i += 1
            }
            try {
              val v = s.toDouble
              tokens = tokens.appended(Token(-1, "", isValue = true, value = v))
            } catch {
              case _: Throwable => tokens = tokens.appended(Token(-1, "", isIdentifier = true, name = s))
            }
            finally {
              i -= 1
            }
          }
        }
      }
      i += 1
    }
    val withoutWS = tokens.filterNot(t => t == Tokens.WHITESPACE)
    withoutWS
  }
}
