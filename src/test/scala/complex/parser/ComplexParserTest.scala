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

  it should "parse a variable expression" in {
    assertResult(VariableExp(Variable("x"))) {
      parse("x", exp)
    }
  }

  it should "parse an int literal" in {
    assertResult(IntLiteralExp(5)) {
      parse("5", exp)
    }
  }

  it should "parse true" in {
    assertResult(BoolLiteralExp(true)) {
      parse("true", exp)
    }
  }

  it should "parse false" in {
    assertResult(BoolLiteralExp(false)) {
      parse("false", exp)
    }
  }

  it should "parse this" in {
    assertResult(ThisExp) {
      parse("this", exp)
    }
  }

  it should "parse array access" in {
    assertResult(
      ArrayAccessExp(
        VariableExp(Variable("arr")),
        IntLiteralExp(0))) {
      parse("arr[0]", exp)
    }
  }

  def assertBinop(opString: String, bop: Bop) {
    assertResult(
      BinaryOperationExp(
        IntLiteralExp(1),
        bop,
        IntLiteralExp(2))) {
      parse(s"1 $opString 2", exp)
    }
  }

  it should "parse addition" in {
    assertBinop("+", PlusBop)
  }

  it should "parse subtraction" in {
    assertBinop("-", MinusBop)
  }

  it should "parse multiplication" in {
    assertBinop("*", MultBop)
  }

  it should "parse division" in {
    assertBinop("/", DivBop)
  }

  it should "parse logical and" in {
    assertBinop("&&", LogicalAndBop)
  }

  it should "parse logical or" in {
    assertBinop("||", LogicalOrBop)
  }

  it should "parse <" in {
    assertBinop("<", LessThanBop)
  }

  it should "parse <=" in {
    assertBinop("<=", LessThanOrEqualsBop)
  }

  it should "parse >" in {
    assertBinop(">", GreaterThanBop)
  }

  it should "parse >=" in {
    assertBinop(">=", GreaterThanOrEqualsBop)
  }

  it should "parse ==" in {
    assertBinop("==", EqualsBop)
  }

  it should "parse !=" in {
    assertBinop("!=", NotEqualsBop)
  }
  
  def assertUnop(opString: String, unop: Unop) {
    assertResult(
      UnaryOperationExp(
        unop,
        IntLiteralExp(3))) {
      parse(s"$opString 3", exp)
    }
  }

  it should "parse logical not" in {
    assertUnop("!", LogicalNotUnop)
  }

  it should "parse unary minus" in {
    assertUnop("-", NegationUnop)
  }
}
