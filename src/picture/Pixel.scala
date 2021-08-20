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

  private def genericArithmeticOperationWithConst (operation: (Double, Double) => Double)(const: Double): Pixel = {
    def genericInner (const: Double): Pixel = {
      val newR = operation(this.r, const)
      val newG = operation(this.g, const)
      val newB = operation(this.b, const)

      new Pixel(newR, newG, newB)
    }
    genericInner(const)
  }

  private def genericFunctionWithConst(operation: (Double, Double) => Double)(const: Double): Pixel =
    genericArithmeticOperationWithConst(operation)(const)

  private def genericFunctionNoConst (operation: Double => Double)(): Pixel = {
    def genericInner: Pixel = {
      val newR = operation(this.r)
      val newG = operation(this.g)
      val newB = operation(this.b)

      new Pixel(newR, newG, newB)
    }
    genericInner
  }

  def add (const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x + y)(const)
  }

  def sub (const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x - y)(const)
  }

  def inverseSub (const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => y - x)(const)
  }

  def mul (const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x * y)(const)
  }

  def div (const: Double): Pixel = {
    if (const == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => x / y)(const)
  }

  def reverseDiv (const: Double): Pixel = {
    if (this.r == 0 || this.g == 0 || this.b == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => y / x)(const)
  }

  def pow (const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.pow(x, y))(const)
  }

  def min (const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.min(x, y))(const)
  }

  def max (const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.max(x, y))(const)
  }

  def log (): Pixel = {
    genericFunctionNoConst(x => math.log(x))()
  }

  def abs (): Pixel = {
    genericFunctionNoConst(x => math.abs(x))()
  }

  def inversion (): Pixel = {
    inverseSub(1)
  }

  def toGrayscale(): Pixel = {
    val avg = (this.r + this.g + this.b) / 3
    new Pixel(avg, avg, avg)
  }

}

object Pixel {
  val VAL_MIN: Double = 0.0
  val VAL_MAX: Double = 1.0
}
