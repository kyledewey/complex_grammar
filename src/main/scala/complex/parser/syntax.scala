package complex.parser

case class Variable(name: String)
case class ClassName(name: String)

sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case class ClassType(name: ClassName) extends Type
case class ArrayType(theType: Type) extends Type
case class FunctionType(paramTypes: Seq[Type], returnType: Type) extends Type

case class Param(theType: Type, variable: Variable)

sealed trait Exp
case class VariableExp(variable: Variable) extends Exp
case class IntLiteralExp(value: Int) extends Exp
case class BoolLiteralExp(value: Boolean) extends Exp
case object ThisExp extends Exp
case class ArrayAccessExp(array: Exp, index: Exp) extends Exp
case class BinaryOperationExp(left: Exp, op: Bop, right: Exp) extends Exp
case class UnaryOperationExp(op: Unop, exp: Exp) extends Exp
case class InstanceofExp(exp: Exp, cls: ClassName) extends Exp
case class ObjectAccessExp(obj: Exp, field: Variable) extends Exp
case class NewObjectExp(cls: ClassName, params: Seq[Exp]) extends Exp
case class NewArrayExp(theType: Type, size: Exp) extends Exp
case class CastExp(theType: Type, exp: Exp) extends Exp
case class FunctionExp(params: Seq[Param], body: Exp) extends Exp
case class CallExp(base: Exp, params: Seq[Exp]) extends Exp

sealed trait Lhs
case class VariableLhs(variable: Variable) extends Lhs
case class ObjectAccessLhs(obj: Exp, field: Variable) extends Lhs
case class ArrayAccessLhs(array: Exp, index: Exp) extends Lhs

sealed trait Bop
case object PlusBop extends Bop
case object MinusBop extends Bop
case object MultBop extends Bop
case object DivBop extends Bop
case object LogicalAndBop extends Bop
case object LogicalOrBop extends Bop
case object LessThanBop extends Bop
case object LessThanOrEqualsBop extends Bop
case object GreaterThanBop extends Bop
case object GreaterThanOrEqualsBop extends Bop
case object EqualsBop extends Bop
case object NotEqualsBop extends Bop

sealed trait Unop
case object LogicalNotUnop extends Unop
case object NegationUnop extends Unop

sealed trait Stmt
case class VardecStmt(param: Param, exp: Exp) extends Stmt
case class AssignStmt(lhs: Lhs, exp: Exp) extends Stmt
case class IfStmt(guard: Exp, ifTrue: Seq[Stmt], ifFalse: Option[Seq[Stmt]]) extends Stmt
case class WhileStmt(guard: Exp, body: Seq[Stmt]) extends Stmt
case class ReturnStmt(exp: Exp) extends Stmt
case class PrintStmt(exp: Exp) extends Stmt

case class ConsDef(
  params: Seq[Param],
  superParams: Option[Seq[Exp]],
  body: Seq[Stmt])

case class MethodDef(
  returnType: Type,
  name: Variable,
  params: Seq[Param],
  body: Seq[Stmt])

case class ClassDef(
  name: ClassName,
  extendsClass: Option[ClassName],
  instanceVariables: Seq[Param],
  constructor: ConsDef,
  methods: Seq[MethodDef])

case class Program(classes: Seq[ClassDef], entryPoint: Seq[Stmt])

