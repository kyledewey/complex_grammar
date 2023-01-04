package complex.tokenizer

sealed trait Token
case class IdentifierToken(name: String) extends Token
case class IntLiteralToken(value: Int) extends Token
case object CommaToken extends Token
case object IntToken extends Token
case object BoolToken extends Token
case object LeftSquareBracketToken extends Token
case object RightSquareBracketToken extends Token
case object LeftParenToken extends Token
case object RightParenToken extends Token
case object ArrowToken extends Token
case object TrueToken extends Token
case object FalseToken extends Token
case object ThisToken extends Token
case object InstanceofToken extends Token
case object DotToken extends Token
case object NewToken extends Token
case object PlusToken extends Token
case object MinusToken extends Token
case object MultToken extends Token
case object DivToken extends Token
case object LogicalAndToken extends Token
case object LogicalOrToken extends Token
case object LessThanToken extends Token
case object LessThanOrEqualsToken extends Token
case object GreaterThanToken extends Token
case object GreaterThanOrEqualsToken extends Token
case object DoubleEqualsToken extends Token
case object NotEqualsToken extends Token
case object LogicalNotToken extends Token
case object SingleEqualsToken extends Token
case object SemicolonToken extends Token
case object LeftCurlyBracketToken extends Token
case object RightCurlyBracketToken extends Token
case object IfToken extends Token
case object ElseToken extends Token
case object WhileToken extends Token
case object ReturnToken extends Token
case object PrintToken extends Token
case object InitToken extends Token
case object SuperToken extends Token
case object ExtendsToken extends Token

