package picture

class Pixel (red: Double,
             green: Double,
             blue: Double,
             checkRange: Boolean = true) {

  def this(color: Double) = this(color, color, color)
  private def checkRange(value: Double): Double = if (value < Pixel.VAL_MIN && checkRange) Pixel.VAL_MIN
                                                  else if (value > Pixel.VAL_MAX && checkRange) Pixel.VAL_MAX
                                                  else value

  val r: Double = checkRange(red)
  val g: Double = checkRange(green)
  val b: Double = checkRange(blue)
}

object Pixel {
  val VAL_MIN: Double = 0.0
  val VAL_MAX: Double = 1.0
}
