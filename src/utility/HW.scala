package utility

class HW (val height: Int,
          val width: Int) {
  require(height >= 0)
  require(width >= 0)

  override def toString: String = {
    s"(H, W) = ($height, $width)"
  }
}
