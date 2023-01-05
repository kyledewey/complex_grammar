package complex.parser

import complex.tokenizer._

import org.scalatest.flatspec.AnyFlatSpec

class ComplexParserTest extends AnyFlatSpec {
  import complex.parser.ComplexParser._
  def parse[A](input: String, parser: Parser[A]): A = {
    parser.parseWhole(Tokenizer.tokenize(input).toList)
  }

  "The parser" should "parse variables" in {
    assertResult(Variable("x")) {
      parse("x", variable)
    }
  }

  it should "parse type int" in {
    assertResult(IntType) {
      parse("int", theType)
    }
  }

  it should "parse type bool" in {
    assertResult(BoolType) {
      parse("bool", theType)
    }
  }

  it should "parse a class type" in {
    assertResult(ClassType(ClassName("Foo"))) {
      parse("Foo", theType)
    }
  }

  it should "parse an array type" in {
    assertResult(ArrayType(IntType)) {
      parse("int[]", theType)
    }
  }

  it should "parse a function type with no params" in {
    assertResult(FunctionType(Seq(), IntType)) {
      parse("() => int", theType)
    }
  }

  it should "parse a function type with one param" in {
    assertResult(FunctionType(Seq(IntType), BoolType)) {
      parse("(int) => bool", theType)
    }
  }

  it should "parse a function type with multiple params" in {
    assertResult(FunctionType(Seq(IntType, BoolType), IntType)) {
      parse("(int, bool) => int", theType)
    }
  }

  it should "parse a function type returning a function" in {
    val expected =
      FunctionType(
        Seq(IntType),
        FunctionType(Seq(BoolType), ClassType(ClassName("Foo"))))
    assertResult(expected) {
      parse("(int) => (bool) => Foo", theType)
    }
  }

  it should "parse a function type involving an array" in {
    assertResult(FunctionType(Seq(IntType), ArrayType(IntType))) {
      parse("(int) => int[]", theType)
    }
  }

  it should "parse an array of functions" in {
    assertResult(ArrayType(FunctionType(Seq(IntType), IntType))) {
      parse("((int) => int)[]", theType)
    }
  }
}
