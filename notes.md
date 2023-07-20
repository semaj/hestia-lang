* Automatic currying
* Infix flow operator
* Immutable everything
* Lists
* Enums
* HashMaps

Expr := Primitive | BooleanExpr | If | Func | Call | Let | Def | <identifier>
Primitive := Number | Boolean | String
BooleanExpr := (and Expr Expr+) | (or Expr Expr+) | (xor Expr Expr+)
If := (if Expr Expr Expr)
Func := { |<identifier>...| Expr }
Call := (<identifier> Expr...)
Let := (let ([<identifier> Expr]...) Expr)
Def := (def <identifier> Expr)

