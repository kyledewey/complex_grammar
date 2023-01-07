# Complex Grammar #

Example showing a grammar that isn't based on S-expressions.

## Abstract Grammar ##

```
i is an integer
var is a variable
clsName is a class name
multiType ::= type | type `,` multiType
types ::= epsilon | multiType
type ::= `int` | `bool` |          // basic types
         clsName |                 // object types
         type `[` `]` |            // array types
         `(` types `)` `=>` type | // higher-order function type
         `(` type `)`              // parenthsitized type
param ::= type var
multiParam ::= param | param `,` multiParam
params ::= epsilon | multiParam
multiExp ::= exp | exp `,` multiExp
exps ::= epsilon | multiExp
exp ::= var | i | `true` | `false` | // AST leaves
        `this` |                     // OOP this
        exp `[` exp `]` |            // array access
        exp bop exp |                // arithmetic operators
        unop exp |                   // unary operators
        exp `instanceof` clsName |   // checking runtime type
        exp `.` var |                // field (instance variable) access
        `new` clsName `(` exps `)` | // creating a new object
        `new` type `[` exp `]`     | // creating a new array
        `(` type `)` exp |           // casting
        `(` params `)` `=>` exp |    // creating higher-order functions
        exp `(` exps `)` |           // calling higher-order functions
        `(` exp `)`                  // parenthesitized expression
lhs ::= var |            // lefthand side of an assignment
        exp `.` var    | // access field
        exp `[` exp `]`  // array access
bop ::= `+` | `-` | `*` | `/` | `&&` | `||` |
        `<` | `<=` | `>` | `>=` | `==` | `!=`
unop ::= `!` | `-`
stmt ::= param `=` exp `;` |                                     // variable declaration
         lhs `=` exp `;` |                                       // assignment
         `if` `(` exp `)` `{` stmt* `}` [`else` `{` stmt* `}`] | // if / else
         `while` `(` exp `)` `{` stmt* `}` |                     // while loops
         `return` exp `;` |                                      // return
         `print` `(` exp `)` `;`                                 // printing
consDef ::= `init` `(` params `)` `{` [`super` `(` exps `)` `;`] stmt* `}`
methodDef ::= type var `(` params `)` `{` stmt* `}`
cls ::= `class` clsName [`extends` clsName] `{` (param `;`)* consDef methodDef* `}
program ::= cls* `{` stmt* `}`
```

## Tokens ##

- IdentifierToken(String)
- IntLiteralToken(int)
- CommaToken: 0
- IntToken: 1
- BoolToken: 2
- LeftSquareBracketToken: 3
- RightSquareBracketToken: 4
- LeftParenToken: 5
- RightParenToken: 6
- ArrowToken: 7
- TrueToken: 8
- FalseToken: 9
- ThisToken: 10
- InstanceofToken: 11
- DotToken: 12
- NewToken: 13
- PlusToken: 14
- MinusToken: 15
- MultToken: 16
- DivToken: 17
- LogicalAndToken: 18
- LogicalOrToken: 19
- LessThanToken: 20
- LessThanOrEqualsToken: 21
- GreaterThanToken: 22
- GreaterThanOrEqualsToken: 23
- DoubleEqualsToken: 24
- NotEqualsToken: 25
- LogicalNotToken: 26
- SingleEqualsToken: 27
- SemicolonToken: 28
- LeftCurlyBracketToken: 29
- RightCurlyBracketToken: 30
- IfToken: 31
- ElseToken: 32
- WhileToken: 33
- ReturnToken: 34
- PrintToken: 35
- InitToken: 36
- SuperToken: 37
- ExtendsToken: 38

## Concrete Grammar (Based on Java Precedence Levels) ##

(int) => (string) => bool

```
i is an integer
var is a variable
clsName is a class name
param ::= type var
multiParam ::= param | param `,` multiParam
params ::= epsilon | multiParam
multiSemiParam ::= param | param `;` multiSemiParam
paramsSemicolon ::= epsilon | multiSemiParam
multiType ::= type | type `,` multiType
types ::= epsilon | multiType
primaryType ::= `int` | `bool` | clsName | `(` type `)`
arrayType ::= primaryType (`[` `]`)*
functionType ::= (`(` types `)` `=>`)* arrayType
type ::= functionType
multiExp ::= exp | exp `,` multiExp
exps ::= epsilon | multiExp
primaryExp ::= var | i | `true` | `false` | `this` | `(` exp `)` |
               `new` clsName `(` exps `)` |
               `new` type `[` exp `]`
dotExp ::= primaryExp (`.` var)*
callOrArrayKind ::= `(` exps `)` | `[` exp `]`
callOrArrayExp ::= dotExp (callOrArrayKind)*
unopExp ::= (`-` | `!`)* callOrArrayExp
castExp ::= (`(` type `)`)* unopExp
multExp ::= castExp ((`*` | `/`) castExp)*
addExp ::= multExp ((`+` | `-`) multExp)*
relationalExp ::= addExp ((`<` | `<=` | `>` | `>=`) addExp)*
equalsKind ::= (`==` | `!=`) relationalExp | `instanceof` clsName
equalsExp ::= relationalExp equalsKind*
andExp ::= equalsExp (`&&` equalsExp)*
orExp ::= andExp (`||` andExp)*
functionExp ::= (`(` params `)` `=>`)* orExp
exp ::= functionExp
lhs ::= var |            // lefthand side of an assignment
        exp `.` var    | // access field
        exp `[` exp `]`  // array access
stmt ::= param `=` exp `;` |                                     // variable declaration
         lhs `=` exp `;` |                                       // assignment
         `if` `(` exp `)` `{` stmt* `}` [`else` `{` stmt* `}`] | // if / else
         `while` `(` exp `)` `{` stmt* `}` |                     // while loops
         `return` exp `;` |                                      // return
         `print` `(` exp `)`                                     // printing
consDef ::= `init` `(` params `)` `{` [`super` `(` exps `)` `;`] stmt* `}`
methodDef ::= type var `(` params `)` `{` stmt* `}`
classDef ::= `class` clsName [`extends` clsName] `{` (param `;`)* consDef methodDef* `}
program ::= classDef* `{` stmt* `}`
```

(Object)new Foo()

Syntactically, there is no way to differentiate between a method
call and a higher-order function call.  For example:

obj.someMethod()(7)
Call(Call(Access(obj,
                 someMethod),
          List()),
     List(7))

...versus:
obj.someMethod(7)

Call(Access(obj, someMethod), List(7))

Is obj.someMethod a method, or is it a function?  No idea - need the typechecker to disambiguate.

((x == y) instanceof Boolean) == z

## Running ##

```console
sbt
runMain complex.Complex examples/class.complex
```
