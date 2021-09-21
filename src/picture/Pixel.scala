package picture

class Pixel (red: Double,
             green: Double,
             blue: Double,
             checkRange: Boolean = true) extends Serializable {

  def this(color: Double) = this(color, color, color)

  private def scaleValue(value: Double): Double = if (value < Pixel.VAL_MIN && checkRange) Pixel.VAL_MIN
                                                  else if (value > Pixel.VAL_MAX && checkRange) Pixel.VAL_MAX
                                                  else value

  val r: Double = scaleValue(red)
  val g: Double = scaleValue(green)
  val b: Double = scaleValue(blue)

  override def toString: String = {
    s"Pixel ($r, $g, $b)\n"
  }
}

object Pixel {
  val VAL_MIN: Double = 0.0
  val VAL_MAX: Double = 1.0

  private def genericArithmeticOperationWithConst(operation: (Double, Double) => Double)(pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    def genericInner(const: Double): Pixel = {
      val newR = operation(pixel.r, const)
      val newG = operation(pixel.g, const)
      val newB = operation(pixel.b, const)

      new Pixel(newR, newG, newB, checkRange)
    }
    genericInner(const)
  }

  private def genericFunctionWithConst(operation: (Double, Double) => Double)(pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel =
    genericArithmeticOperationWithConst(operation)(pixel, const, checkRange)

  private def genericFunctionNoConst(operation: Double => Double)(pixel: Pixel, checkRange: Boolean = true): Pixel = {
    def genericInner(): Pixel = {
      val newR = operation(pixel.r)
      val newG = operation(pixel.g)
      val newB = operation(pixel.b)

      new Pixel(newR, newG, newB, checkRange)
    }
    genericInner()
  }

  def add (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x + y)(pixel, const, checkRange)
  }

  def sub (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x - y)(pixel, const, checkRange)
  }

  def inverseSub (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericArithmeticOperationWithConst((x, y) => y - x)(pixel, const, checkRange)
  }

  def mul (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x * y)(pixel, const, checkRange)
  }

  def div (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    if (const == 0) return pixel
//    if (const == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => x / y)(pixel, const, checkRange)
  }

  def inverseDiv (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    if (pixel.r == 0 || pixel.g == 0 || pixel.b == 0) return pixel
//    if (pixel.r == 0 || pixel.g == 0 || pixel.b == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => y / x)(pixel, const, checkRange)
  }

  def pow (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericFunctionWithConst((x, y) => math.pow(x, y))(pixel, const, checkRange)
  }

  def min (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericFunctionWithConst((x, y) => math.min(x, y))(pixel, const, checkRange)
  }

  def max (pixel: Pixel, const: Double, checkRange: Boolean = true): Pixel = {
    genericFunctionWithConst((x, y) => math.max(x, y))(pixel, const, checkRange)
  }

  def log (pixel: Pixel, checkRange: Boolean = true): Pixel = {
    genericFunctionNoConst(x => math.log(x))(pixel, checkRange)
  }

  def abs (pixel: Pixel, checkRange: Boolean = true): Pixel = {
    genericFunctionNoConst(x => math.abs(x))(pixel, checkRange)
  }

  def grayscale (pixel: Pixel, checkRange: Boolean = true): Pixel = {
    val avg = (pixel.r + pixel.g + pixel.b) / 3
    new Pixel(avg, avg, avg, checkRange)
  }

  def applyColor (pixel: Pixel, red: Double, green: Double, blue: Double, checkRange: Boolean = true): Pixel = {
    new Pixel(red, green, blue, checkRange)
  }
}
