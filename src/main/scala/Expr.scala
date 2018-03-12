import Element.elem

sealed abstract class Expr {
  def simplify(): Expr = {
      this match {
        case UnOp(UnOp(e, "-"), "-") => e
        case BinOp(e, Num(0), "+") => e
        case BinOp(e, Num(0), "-") => e
        case BinOp(e, Num(0), "*") => Num(0)
        case BinOp(e, Num(1), "*") => e
        case _ => this
      }
  }

  val operations = Array(
    Set("+", "-"),
    Set("*", "/")
  )

  val precedence = {
    val a = for {
      i <- 0 until operations.length
      op <- operations(i)
    } yield (op, i)
    a.toMap
  }

  override def toString: String = {
    this.toString(0)
  }

  private def toString(outerPrec:Int):String = {
    this match {
      case Var(name) => name
      case Num(i : Int) => i.toString
      case BinOp(a, b, "/") => {
        val elemA     : Element = elem(a.toString(0))
        val elemB     : Element = elem(b.toString(0))
        val fracDelim : Element = elem('-', 1, elemA.contents(0).length max elemB.contents(0).length)
        (elemA above fracDelim above elemB).toString
      }
      case BinOp(a, b, op) => {
        val innerPrec = precedence(op)
        val elemA     : Element = elem(a.toString(innerPrec))
        val elemB     : Element = elem(b.toString(innerPrec))
        val res = (elemA beside elem(op) beside elemB).toString
        if (innerPrec < outerPrec)
          s"(${res})"
        else
          res
      }
      case UnOp(a, op) => s"${op} ${a.toString}"
    }
  }

}

case class Var(name: String) extends Expr
case class Num(n: Int) extends Expr
case class BinOp(a: Expr, b: Expr, op: String) extends Expr
case class UnOp(a: Expr, op: String) extends Expr