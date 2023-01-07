package complex

import complex.tokenizer._
import complex.parser._

object Complex {
  def usage(): Unit = {
    println("Takes a file to parse")
  }

  def main(args: Array[String]) {
    if (args.length != 1) {
      usage()
    } else {
      val input = scala.io.Source.fromFile(args(0)).mkString
      val tokens = Tokenizer.tokenize(input)
      val ast = ComplexParser.program.parseWhole(tokens.toList)
      println(ast)
    }
  }
}
