class Express {

}

object Express {

  def main(args: Array[String]) : Unit = {

    val a = new BinOp(new Num(1), new Num(2), "/")
    val b = new BinOp(a, new Num(3), "/")
    val a1 = new BinOp(a, new Var("x"), "+")
    val c = new BinOp(new Var("z"), a1, "*")
    val d = new BinOp(b, c, "+")
    val e = new BinOp(d, c, "/")
    val e1 = new BinOp(new Var("log(ololo)"), e, "+")

    //println(c)
    println(new BinOp(e1, e1, "*"))

  }

}
