package utility

class Point (val x: Int,
             val y: Int) extends Serializable {
  require(x >= 0)
  require(y >= 0)

  override def toString: String = {
    s"(x, y) = ($x, $y)"
  }
}
