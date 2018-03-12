import Element.elem

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length

  def above(that: Element): Element = {
    val maxWidth = this.contents(0).length.max(that.contents(0).length)
    elem(this.widthen(maxWidth) ++ that.widthen(maxWidth))
  }

  def beside(that: Element): Element = {

    def concat(a: Array[String], b: Array[String], i:Int): Array[String] = {
      if (i == -1)
        Array[String]()
      else
        concat(a, b, i-1) :+ (a(i) ++ b(i))
    }

    val maxHeight = this.contents.length.max(that.contents.length)
    elem(
      concat(
        this.heighten(maxHeight)
        , that.heighten(maxHeight)
        , maxHeight - 1
      )
    )
  }

  def heighten(maxHeight: Int): Array[String] = {

    if (height == maxHeight) return this.contents

    val ws			= " "
    val upperAdd	= Array.fill((maxHeight - height) / 2){ws}
    val bottomAdd	= Array.fill((maxHeight - height) / 2 + (maxHeight - height) % 2){ws}
    upperAdd ++ contents ++ bottomAdd
  }

  def widthen(maxWidth: Int): Array[String] = {

    if (width == maxWidth) return this.contents

    val ws			= " "
    val leftAdd	= ws * ((maxWidth - width) / 2)
    val rightAdd	= ws * ((maxWidth - width) / 2 + (maxWidth - width) % 2)
    this.contents.map(leftAdd + (_:String) + rightAdd)
  }

  override def toString: String = contents mkString "\n"
}

object Element {

  private class ArrayElement(val contents: Array[String]) extends Element {
  }

  private class FillElement(
                     ch: Char
                     , override val height: Int
                     , override val width: Int
                   ) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }


  private class LineElement(s: String) extends Element {
    override def contents: Array[String] = Array(s)
    override def height: Int = 1
  }

  def elem(contents: Array[String]):Element = { new ArrayElement(contents) }

  def elem(s: String):Element = { new LineElement(s) }

  def elem(ch: Char, height: Int, width: Int):Element = { new FillElement(ch, height, width) }

}