package utility

class Rectangle (val topLeftCorner: Point,
                 val dim: HW) extends Serializable {
  override def toString: String = {
    s"$topLeftCorner\n$dim"
  }
}