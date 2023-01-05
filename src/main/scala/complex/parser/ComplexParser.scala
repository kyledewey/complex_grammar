package complex.parser

import complex.tokenizer._

object ComplexParser {
  import Parser._

  type P[+A] = Parser[A]

  lazy val num: P[Int] = lift({ case IntLiteralToken(i) => i }, "int literal")
  lazy val identifier: P[String] = lift({ case IdentifierToken(s) => s }, "identfier")
  lazy val variable: P[Variable] = identifier ^^ (Variable.apply _)
  lazy val className: P[ClassName] = identifier ^^ (ClassName.apply _)

  lazy val param: P[Param] =
    theType ~ variable ^^ { case t ~ v => Param(t, v) }

  def commaSep[A](p: => P[A]): Parser[List[A]] = {
    repsep(p, token(CommaToken))
  }

  lazy val params: P[Seq[Param]] = commaSep(param)

  def inSomething[A](
    left: => P[Unit],
    thing: => P[A],
    right: => P[Unit]): P[A] = {
    left ~ thing ~ right ^^ { case _ ~ a ~ _ => a }
  }

  def inParens[A](p: => P[A]): P[A] = {
    inSomething(token(LeftParenToken), p, token(RightParenToken))
  }

  def inSquareBrackets[A](p: => P[A]): P[A] = {
    inSomething(token(LeftSquareBracketToken), p, token(RightSquareBracketToken))
  }

  def inCurlyBrackets[A](p: => P[A]): P[A] = {
    inSomething(token(LeftCurlyBracketToken), p, token(RightCurlyBracketToken))
  }

  lazy val primaryType: P[Type] = {
    (token(IntToken) ^^^ IntType) |
    (token(BoolToken) ^^^ BoolType) |
    (className ^^ (ClassType.apply _)) |
    inParens(theType)
  }

  lazy val arrayType: P[Type] = {
    primaryType ~ rep(token(LeftSquareBracketToken) ~ token(RightSquareBracketToken)) ^^
    { case t ~ brackets =>
      brackets.foldLeft(t)((accum, _) => ArrayType(accum)) }
  }

  lazy val functionType: P[Type] = {
    rep(inParens(types) ~ token(ArrowToken)) ~ arrayType ^^
    { case paramGroups ~ returnType =>
      paramGroups.foldRight(returnType)((curGroup, accum) =>
        FunctionType(curGroup._1, accum)) }
  }

  lazy val theType: P[Type] = functionType

  lazy val types: P[Seq[Type]] = commaSep(theType)

  lazy val exps: P[Seq[Exp]] = commaSep(exp)

  lazy val primaryExp: P[Exp] = {
    (variable ^^ (VariableExp.apply _)) |
    (num ^^ (IntLiteralExp.apply _)) |
    (token(TrueToken) ^^^ BoolLiteralExp(true)) |
    (token(FalseToken) ^^^ BoolLiteralExp(false)) |
    (token(ThisToken) ^^^ ThisExp) |
    inParens(exp) |
    (token(NewToken) ~ className ~ inParens(exps) ^^
      { case _ ~ cls ~ exps => NewObjectExp(cls, exps) }) |
    (token(NewToken) ~ theType ~ inSquareBrackets(exp) ^^
      { case _ ~ typ ~ exp => NewArrayExp(typ, exp) })
  } // primaryExp

  lazy val dotExp: P[Exp] = {
    primaryExp ~ rep(token(DotToken) ~ variable) ^^
    { case exp ~ dots => dots.foldLeft(exp)((accum, cur) =>
      ObjectAccessExp(accum, cur._2)) }
  }

  sealed trait CallOrArrayKind {
    def toExp(base: Exp): Exp
  }
  case class CallKind(exps: Seq[Exp]) extends CallOrArrayKind {
    def toExp(base: Exp): Exp = CallExp(base, exps)
  }
  case class ArrayKind(exp: Exp) extends CallOrArrayKind {
    def toExp(base: Exp): Exp = ArrayAccessExp(base, exp)
  }

  lazy val callOrArrayKind: P[CallOrArrayKind] = {
    (inParens(exps) ^^ (CallKind.apply _)) |
    inSquareBrackets(exp) ^^ (ArrayKind.apply _)
  }

  lazy val callOrArrayExp: P[Exp] = {
    dotExp ~ rep(callOrArrayKind) ^^
    { case base ~ reps => reps.foldLeft(base)((accum, cur) =>
      cur.toExp(accum)) }
  }

  lazy val unop: P[Unop] = {
    (token(MinusToken) ^^^ NegationUnop) |
    (token(LogicalNotToken) ^^^ LogicalNotUnop)
  }

  lazy val unopExp: P[Exp] = {
    rep(unop) ~ callOrArrayExp ^^
    { case unops ~ base => unops.foldRight(base)((cur, accum) =>
      UnaryOperationExp(cur, accum)) }
  }

  lazy val castExp: P[Exp] = {
    rep(inParens(theType)) ~ unopExp ^^
    { case casts ~ base => casts.foldRight(base)((cur, accum) =>
      CastExp(cur, accum)) }
  }

  def binop(levelUp: => P[Exp], bop: => P[Bop]): P[Exp] = {
    levelUp ~ rep(bop ~ levelUp) ^^
    { case base ~ reps => reps.foldLeft(base)((accum, cur) =>
      BinaryOperationExp(accum, cur._1, cur._2)) }
  }

  lazy val multExp: P[Exp] = {
    binop(
      castExp,
      (token(MultToken) ^^^ MultBop) |
        (token(DivToken) ^^^ DivBop))
  }

  lazy val addExp: P[Exp] = {
    binop(
      multExp,
      (token(PlusToken) ^^^ PlusBop) |
        (token(MinusToken) ^^^ MinusBop))
  }

  lazy val relationalExp: P[Exp] = {
    binop(
      addExp,
      (token(LessThanToken) ^^^ LessThanBop) |
        (token(LessThanOrEqualsToken) ^^^ LessThanOrEqualsBop) |
        (token(GreaterThanToken) ^^^ GreaterThanBop) |
        (token(GreaterThanOrEqualsToken) ^^^ GreaterThanOrEqualsBop))
  }

  sealed trait EqualsKind {
    def toExp(left: Exp): Exp
  }
  case class EqualsEqualsKind(bop: Bop, exp: Exp) extends EqualsKind {
    def toExp(left: Exp): Exp = BinaryOperationExp(left, bop, exp)
  }
  case class InstanceofEqualsKind(className: ClassName) extends EqualsKind {
    def toExp(left: Exp): Exp = InstanceofExp(left, className)
  }

  lazy val equalsBop: P[Bop] = {
    (token(DoubleEqualsToken) ^^^ EqualsBop) |
    (token(NotEqualsToken) ^^^ NotEqualsBop)
  }

  lazy val equalsKind: P[EqualsKind] = {
    (equalsBop ~ relationalExp ^^
      { case bop ~ exp => EqualsEqualsKind(bop, exp) }) |
    (token(InstanceofToken) ~ className ^^
      { case _ ~ name => InstanceofEqualsKind(name) })
  }

  lazy val equalsExp: P[Exp] = {
    relationalExp ~ rep(equalsKind) ^^
    { case base ~ reps => reps.foldLeft(base)((accum, cur) =>
      cur.toExp(accum)) }
  }

  lazy val andExp: P[Exp] = {
    binop(
      equalsExp,
      token(LogicalAndToken) ^^^ LogicalAndBop)
  }

  lazy val orExp: P[Exp] = {
    binop(
      andExp,
      token(LogicalOrToken) ^^^ LogicalOrBop)
  }

  lazy val functionExp: P[Exp] = {
    rep(inParens(params) ~ token(ArrowToken)) ~ orExp ^^
    { case reps ~ body => reps.foldRight(body)((cur, accum) =>
      FunctionExp(cur._1, accum)) }
  }

  lazy val exp: P[Exp] = functionExp

  // lhs is very ambiguous, and the fact that exp is intermixed
  // makes it worse.  Trick: parse an expression, and convert to
  // the corresponding lhs, if possible
  lazy val lhs: P[Lhs] = {
    exp.flatMap(e => {
      e match {
        case VariableExp(x) => success(VariableLhs(x))
        case ObjectAccessExp(e, f) => success(ObjectAccessLhs(e, f))
        case ArrayAccessExp(e1, e2) => success(ArrayAccessLhs(e1, e2))
        case other => failure("Expected lhs; received: " + other)
      }
    })
  }

  lazy val vardecStmt: P[VardecStmt] = {
    param ~ token(SingleEqualsToken) ~ exp ~ token(SemicolonToken) ^^
    { case param ~ _ ~ exp ~ _ => VardecStmt(param, exp) }
  }

  lazy val assignStmt: P[AssignStmt] = {
    lhs ~ token(SingleEqualsToken) ~ exp ~ token(SemicolonToken) ^^
    { case lhs ~ _ ~ exp ~ _ => AssignStmt(lhs, exp) }
  }

  lazy val ifStmt: P[IfStmt] = {
    token(IfToken) ~ inParens(exp) ~ inCurlyBrackets(stmts) ~ opt(token(ElseToken) ~ inCurlyBrackets(stmts)) ^^
    { case _ ~ guard ~ ifTrue ~ opIfFalse =>
      IfStmt(guard, ifTrue, opIfFalse.map(_._2)) }
  }

  lazy val whileStmt: P[WhileStmt] = {
    token(WhileToken) ~ inParens(exp) ~ inCurlyBrackets(stmts) ^^
    { case _ ~ guard ~ body => WhileStmt(guard, body) }
  }

  lazy val returnStmt: P[ReturnStmt] = {
    token(ReturnToken) ~ exp ~ token(SemicolonToken) ^^
    { case _ ~ exp ~ _ => ReturnStmt(exp) }
  }

  lazy val printStmt: P[PrintStmt] = {
    token(PrintToken) ~ exp ~ token(SemicolonToken) ^^
    { case _ ~ exp ~ _ => PrintStmt(exp) }
  }

  lazy val stmts: P[Seq[Stmt]] = rep(stmt)

  lazy val stmt: P[Stmt] = {
    assignStmt |
    vardecStmt |
    ifStmt |
    whileStmt |
    returnStmt |
    printStmt
  }

  lazy val consDef: P[ConsDef] = {
    token(InitToken) ~
    inParens(rep(param)) ~
    inCurlyBrackets(opt(token(SuperToken) ~ inParens(exps) ~ token(SemicolonToken)) ~ stmts) ^^
    { case _ ~ params ~ (opSuper ~ body) =>
      ConsDef(params, opSuper.map(_._1._2), body) }
  }

  lazy val methodDef: P[MethodDef] = {
    theType ~
    variable ~
    inParens(params) ~
    inCurlyBrackets(stmts) ^^
    { case returnType ~ name ~ params ~ body =>
      MethodDef(returnType, name, params, body) }
  }

  lazy val classDef: P[ClassDef] = {
    token(ClassToken) ~
    className ~
    opt(token(ExtendsToken) ~ className) ~
    inCurlyBrackets(rep(param ~ token(SemicolonToken)) ~ consDef ~ rep(methodDef)) ^^
    { case _ ~ name ~ opExtends ~ (params ~ consDef ~ methods) =>
      ClassDef(name, opExtends.map(_._2), params.map(_._1), consDef, methods) }
  }

  lazy val program: P[Program] = {
    rep(classDef) ~ inCurlyBrackets(stmts) ^^
    { case classes ~ entryPoint => Program(classes, entryPoint) }
  }
} // ComplexParser
