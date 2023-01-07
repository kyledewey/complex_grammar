# Concrete #

This was an example done together in the video.
Not actually the syntax used (see README.md).

# Types #
```
primaryType ::= `int` | `bool` | clsName | `(` type `)`
arrayType ::= primaryType (`[` `]`)*
functionType ::= (`(` types `)` `)` `=>`)* arrayType
type ::= functionType
```

# Expressions #

```
(int) => (int) => int f = (int x) => (int y) => x + y;
int added = f(2)(3);
int[][] matrix = ...;
(matrix[row])[col]
(obj.foo).bar

(((obj.firstMethod)()).secondMethod)()
!!x
```

```
primaryExp ::= var | i | `true` | `false` | `this` | `(` exp `)` |
               `new` clsName `(` exps `)` |
               `new` type `[` exp `]`
prec16kind ::= `(` exps `)` | `[` exp `]` | `.` var
prec16 ::= primaryExp (prec16kind)*
unaryExp ::= (`!` | `-`)* prec16
castExp ::= (`(` type `)`)* unaryExp
multExp ::= castExp ((`*` | `/`) castExp)*
addExp :: = multExp ((`+` | `-`) multExp)*
prec9kind ::= ((`<` | `<=` | `>` | `>=`) addExp) | `instanceof` className
prec9 ::= addExp (prec9kind)*
equalsExp ::= prec9 ((`==` | `!=`) prec9)*
andExp ::= equalsExp (`&&` equalsExp)*
orExp ::= andExp (`||` andExp)*
functionExp ::= (`(` params `)` `=>`)* orExp
exp ::= functionExp
```
