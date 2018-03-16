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
        val ws        : Element = elem(" ")
        val elemA     : Element = elem(a.toString(0))
        val elemB     : Element = elem(b.toString(0))
        val maxHeight : Int = elemA.contents.length max elemB.contents.length
        val maxLength : Int = elemA.contents(0).length max elemB.contents(0).length
        val elemA1    : Element = elem(elemA.heighten(maxHeight))
        val elemB1    : Element = elem(elemB.heighten(maxHeight))
        val fracDelim : Element = ws beside elem('-', 1, maxLength) beside ws
        (elemA1 above fracDelim above elemB1).toString
      }
      case BinOp(a, b, op) => {
        val innerPrec = precedence(op)
        val ws        : Element = elem(" ")
        val elemA     : Element = elem(a.toString(innerPrec))
        val elemB     : Element = elem(b.toString(innerPrec))
        val res = elemA beside ws beside elem(op) beside ws beside elemB
        if (innerPrec < outerPrec) {
          val prntsL = elem("(")
          val prntsR = elem(")")
          (prntsL beside res beside prntsR).toString
        }
        else
          res.toString
      }
      case UnOp(a, op) => s"${op} ${a.toString}"
    }
  }

}

case class Var(name: String) extends Expr
case class Num(n: Int) extends Expr
case class BinOp(a: Expr, b: Expr, op: String) extends Expr
case class UnOp(a: Expr, op: String) extends Expr