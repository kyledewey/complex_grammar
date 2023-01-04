package complex.tokenizer

class TokenizerException(message: String) extends Exception(message)

object Tokenizer {
  val symbols: Seq[(String, Token)] =
    Seq(
      (",", CommaToken),
      ("[", LeftSquareBracketToken),
      ("]", RightSquareBracketToken),
      ("(", LeftParenToken),
      (")", RightParenToken),
      ("=>", ArrowToken),
      (".", DotToken),
      ("+", PlusToken),
      ("-", MinusToken),
      ("*", MultToken),
      ("/", DivToken),
      ("&&", LogicalAndToken),
      ("||", LogicalOrToken),
      ("<=", LessThanOrEqualsToken),
      ("<", LessThanToken),
      (">=", GreaterThanOrEqualsToken),
      (">", GreaterThanToken),
      ("==", DoubleEqualsToken),
      ("!=", NotEqualsToken),
      ("!", LogicalNotToken),
      ("=", SingleEqualsToken),
      (";", SemicolonToken),
      ("{", LeftCurlyBracketToken),
      ("}", RightCurlyBracketToken))

  val reservedWords: Map[String, Token] =
    Map(
      ("int", IntToken),
      ("bool", BoolToken),
      ("true", TrueToken),
      ("false", FalseToken),
      ("this", ThisToken),
      ("instanceof", InstanceofToken),
      ("new", NewToken),
      ("if", IfToken),
      ("else", ElseToken),
      ("while", WhileToken),
      ("return", ReturnToken),
      ("init", InitToken),
      ("super", SuperToken),
      ("extends", ExtendsToken))
} // Tokenizer

class Tokenizer(val input: String) {
  def getChar(pos: Int): Option[Char] = {
    if (pos >= 0 && pos < input.length) {
      Some(input.charAt(pos))
    } else {
      None
    }
  } // getChar

  def getChars(pos: Int, predicate: Char => Boolean): List[Char] = {
    def loop(curPos: Int, accum: List[Char]): List[Char] = {
      getChar(pos).filter(predicate).map(c =>
        loop(curPos + 1, c :: accum)).getOrElse(accum.reverse)
    }
    loop(pos, List())
  } // getChars

  def tokenizeNumber(pos: Int): Option[(Token, Int)] = {
    val digits = getChars(pos, Character.isDigit _)
    if (digits.nonEmpty) {
      Some((IntLiteralToken(digits.mkString.toInt), pos + digits.size))
    } else {
      None
    }
  } // tokenizeNumber

  def tokenizeSymbol(pos: Int): Option[(Token, Int)] = {
    Tokenizer.symbols.find(pair => input.startsWith(pair._1, pos)).map(pair =>
      (pair._2, pos + pair._1.length))
  }

  def tokenizeReservedWordOrIdentifier(pos: Int): Option[(Token, Int)] = {
    getChar(pos).filter(Character.isLetter _).map(start => {
      val name = (start :: getChars(pos + 1, Character.isLetterOrDigit _)).mkString
      (Tokenizer.reservedWords.getOrElse(name, IdentifierToken(name)), pos + name.length)
    })
  }

  def skipWhitespace(pos: Int): Int = {
    pos + getChars(pos, Character.isWhitespace _).size
  }

  def tokenizeSingle(pos: Int): (Token, Int) = {
    tokenizeNumber(pos).orElse(
      tokenizeSymbol(pos).orElse(
        tokenizeReservedWordOrIdentifier(pos))).getOrElse(
      throw new TokenizerException("Could not tokenize at position " + pos))
  }

  def tokenize(pos: Int): Seq[Token] = {
    @scala.annotation.tailrec
    def loop(curPos: Int, accum: List[Token]): List[Token] = {
      val afterWhitespace = skipWhitespace(curPos)
      if (afterWhitespace >= input.length) {
        accum.reverse
      } else {
        val (token, nextPos) = tokenizeSingle(afterWhitespace)
        loop(nextPos, token :: accum)
      }
    } // loop

    loop(pos, List())
  } // tokenize

  def tokenize(): Seq[Token] = tokenize(0)
} // Tokenizer
