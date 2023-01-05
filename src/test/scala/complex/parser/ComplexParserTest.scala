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

  it should "parse instanceof" in {
    assertResult(
      InstanceofExp(
        VariableExp(Variable("x")),
        ClassName("Foo"))) {
      parse("x instanceof Foo", exp)
    }
  }

  it should "parse object access" in {
    assertResult(
      ObjectAccessExp(
        VariableExp(Variable("foo")),
        Variable("bar"))) {
      parse("foo.bar", exp)
    }
  }

  it should "parse new object creation" in {
    assertResult(
      NewObjectExp(
        ClassName("MyClass"),
        Seq(IntLiteralExp(1), IntLiteralExp(2)))) {
      parse("new MyClass(1, 2)", exp)
    }
  }

  it should "parse new array creation" in {
    assertResult(
      NewArrayExp(
        ClassType(ClassName("MyClass")),
        IntLiteralExp(5))) {
      parse("new MyClass[5]", exp)
    }
  }

  it should "parse casting" in {
    assertResult(
      CastExp(
        ClassType(ClassName("MyClass")),
        VariableExp(Variable("obj")))) {
      parse("(MyClass)obj", exp)
    }
  }

  it should "parse multiple casts" in {
    assertResult(
      CastExp(
        ClassType(ClassName("Object")),
        CastExp(
          ClassType(ClassName("MyClass")),
          VariableExp(Variable("obj"))))) {
      parse("(Object)(MyClass)obj", exp)
    }
  }

  it should "parse function creation" in {
    assertResult(
      FunctionExp(
        Seq(Param(IntType, Variable("x"))),
        VariableExp(Variable("x")))) {
      parse("(int x) => x", exp)
    }
  }

  it should "parse nested function creation" in {
    assertResult(
      FunctionExp(
        Seq(Param(IntType, Variable("x"))),
        FunctionExp(
          Seq(Param(BoolType, Variable("y"))),
          BinaryOperationExp(
            VariableExp(Variable("x")),
            PlusBop,
            VariableExp(Variable("y")))))) {
      parse("(int x) => (bool y) => x + y", exp)
    }
  }

  it should "parse a basic call" in {
    assertResult(
      CallExp(
        VariableExp(Variable("foo")),
        Seq(VariableExp(Variable("bar"))))) {
      parse("foo(bar)", exp)
    }
  }

  it should "parse a method-like call" in {
    assertResult(
      CallExp(
        ObjectAccessExp(
          VariableExp(Variable("obj")),
          Variable("myMethod")),
        Seq(VariableExp(Variable("foo"))))) {
      parse("obj.myMethod(foo)", exp)
    }
  }

  it should "parse a chain of calls" in {
    assertResult(
      CallExp(
        CallExp(
          CallExp(
            VariableExp(Variable("foo")),
            Seq(VariableExp(Variable("bar")))),
          Seq(VariableExp(Variable("baz")))),
        Seq(VariableExp(Variable("blah"))))) {
      parse("foo(bar)(baz)(blah)", exp)
    }
  }

  it should "parse parenthesized expressions" in {
    assertResult(
      BinaryOperationExp(
        IntLiteralExp(2),
        MultBop,
        BinaryOperationExp(
          IntLiteralExp(3),
          PlusBop,
          IntLiteralExp(4)))) {
      parse("2 * (3 + 4)", exp)
    }
  }

  it should "handle precedence - array access vs. object access" in {
    assertResult(
      ArrayAccessExp(
        ObjectAccessExp(
          VariableExp(Variable("foo")),
          Variable("blah")),
        IntLiteralExp(5))) {
      parse("foo.blah[5]", exp)
    }
  }

  it should "handle precedence - calls vs. array access" in {
    assertResult(
      ArrayAccessExp(
        CallExp(
          ArrayAccessExp(
            CallExp(
              VariableExp(Variable("foo")),
              Seq(IntLiteralExp(1))),
            IntLiteralExp(2)),
          Seq(IntLiteralExp(3))),
        IntLiteralExp(4))) {
      parse("foo(1)[2](3)[4]", exp)
    }
  }

  it should "handle precedence - unary vs. binary operations" in {
    assertResult(
      BinaryOperationExp(
        UnaryOperationExp(
          LogicalNotUnop,
          VariableExp(Variable("x"))),
        LogicalAndBop,
        BoolLiteralExp(true))) {
      parse("!x && true", exp)
    }
  }

  it should "handle precedence - multiplications vs. addition" in {
    assertResult(
      BinaryOperationExp(
        BinaryOperationExp(
          IntLiteralExp(1),
          PlusBop,
          BinaryOperationExp(
            IntLiteralExp(2),
            MultBop,
            IntLiteralExp(3))),
        MinusBop,
        IntLiteralExp(4))) {
      // (1 + (2 * 3)) - 4
      parse("1 + 2 * 3 - 4", exp)
    }
  }

  it should "handle precedence - addition vs. multiplication vs. relational" in {
    assertResult(
      BinaryOperationExp(
        BinaryOperationExp(
          IntLiteralExp(1),
          PlusBop,
          IntLiteralExp(2)),
        LessThanBop,
        BinaryOperationExp(
          IntLiteralExp(3),
          MultBop,
          IntLiteralExp(4)))) {
      parse("1 + 2 < 3 * 4", exp)
    }
  }

  it should "handle precedence - relational vs. equals" in {
    assertResult(
      BinaryOperationExp(
        BinaryOperationExp(
          VariableExp(Variable("x")),
          LessThanBop,
          VariableExp(Variable("y"))),
        EqualsBop,
        BinaryOperationExp(
          VariableExp(Variable("a")),
          GreaterThanBop,
          VariableExp(Variable("b"))))) {
      parse("x < y == a > b", exp)
    }
  }

  it should "handle precedence - instanceof vs. equals" in {
    assertResult(
      BinaryOperationExp(
        InstanceofExp(
          BinaryOperationExp(
            VariableExp(Variable("x")),
            EqualsBop,
            VariableExp(Variable("y"))),
          ClassName("MyClass")),
        EqualsBop,
        VariableExp(Variable("z")))) {
      // ((x == y) instanceof MyClass) == z
      parse("x == y instanceof MyClass == z", exp);
    }
  }

  it should "handle precedence - && vs. ||" in {
    assertResult(
      BinaryOperationExp(
        BinaryOperationExp(
          BinaryOperationExp(
            VariableExp(Variable("x")),
            LogicalAndBop,
            VariableExp(Variable("y"))),
          LogicalOrBop,
          BinaryOperationExp(
            VariableExp(Variable("a")),
            LogicalAndBop,
            VariableExp(Variable("b")))),
        LogicalOrBop,
        BinaryOperationExp(
          VariableExp(Variable("c")),
          LogicalAndBop,
          VariableExp(Variable("d"))))) {
      // ((x && y) || (a && b)) || (c && d)
      parse("x && y || a && b || c && d", exp)
    }
  }

  it should "handle lhs - variables" in {
    assertResult(VariableLhs(Variable("x"))) {
      parse("x", lhs)
    }
  }

  it should "handle lhs - object access" in {
    assertResult(
      ObjectAccessLhs(
        VariableExp(Variable("obj")),
        Variable("x"))) {
      parse("obj.x", lhs)
    }
  }

  it should "handle lhs - array access" in {
    assertResult(
      ArrayAccessLhs(
        VariableExp(Variable("arr")),
        VariableExp(Variable("x")))) {
      parse("arr[x]", lhs)
    }
  }

  it should "handle vardec" in {
    assertResult(
      VardecStmt(Param(IntType, Variable("x")), IntLiteralExp(7))) {
      parse("int x = 7;", stmt)
    }
  }

  it should "handle assignment" in {
    assertResult(
      AssignStmt(
        VariableLhs(Variable("x")),
        VariableExp(Variable("y")))) {
      parse("x = y;", stmt)
    }
  }

  it should "handle if - no else" in {
    assertResult(
      IfStmt(
        BoolLiteralExp(true),
        Seq(VardecStmt(Param(IntType, Variable("x")), IntLiteralExp(7))),
        None)) {
      parse("if (true) { int x = 7; }", stmt)
    }
  }

  it should "handle if - with else" in {
    assertResult(
      IfStmt(
        BoolLiteralExp(true),
        Seq(VardecStmt(Param(IntType, Variable("x")), IntLiteralExp(7))),
        Some(Seq(VardecStmt(Param(IntType, Variable("x")), IntLiteralExp(8)))))) {
      parse("if (true) { int x = 7; } else { int x = 8; }", stmt)
    }
  }

  it should "handle while" in {
    assertResult(
      WhileStmt(
        BoolLiteralExp(true),
        Seq(VardecStmt(Param(IntType, Variable("x")), IntLiteralExp(7))))) {
      parse("while (true) { int x = 7; }", stmt)
    }
  }

  it should "handle return" in {
    assertResult(
      ReturnStmt(IntLiteralExp(7))) {
      parse("return 7;", stmt)
    }
  }

  it should "handle print" in {
    assertResult(
      PrintStmt(IntLiteralExp(7))) {
      parse("print(7);", stmt)
    }
  }
}
