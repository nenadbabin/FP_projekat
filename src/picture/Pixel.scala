package picture

class Pixel (red: Double,
             green: Double,
             blue: Double) {

  def this(color: Double) = this(color, color, color)

  val r: Double = if (red < Pixel.VAL_MIN) Pixel.VAL_MIN else if (red > Pixel.VAL_MAX) Pixel.VAL_MAX else red
  val g: Double = if (green < Pixel.VAL_MIN) Pixel.VAL_MIN else if (green > Pixel.VAL_MAX) Pixel.VAL_MAX else green
  val b: Double = if (blue < Pixel.VAL_MIN) Pixel.VAL_MIN else if (blue > Pixel.VAL_MAX) Pixel.VAL_MAX else blue

  private def genericArithmeticOperation (operation: (Double, Double) => Double)(const: Double): Pixel = {
    def genericInner (const: Double): Pixel = {
      val newR = operation(this.r, const)
      val newG = operation(this.g, const)
      val newB = operation(this.b, const)

      new Pixel(newR, newG, newB)
    }
    genericInner(const)
  }

  private def genericFunction (operation: Double => Double)(): Pixel = {
    def genericInner: Pixel = {
      val newR = operation(this.r)
      val newG = operation(this.g)
      val newB = operation(this.b)

      new Pixel(newR, newG, newB)
    }
    genericInner
  }

  def add (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => x + y)(const)
  }

  def sub (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => x - y)(const)
  }

  def inverseSub (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => y - x)(const)
  }

  def mul (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => x * y)(const)
  }

  def div (const: Double): Pixel = {
    if (const == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperation((x, y) => x / y)(const)
  }

  def reverseDiv (const: Double): Pixel = {
    if (this.r == 0 || this.g == 0 || this.b == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperation((x, y) => y / x)(const)
  }

  def pow (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => math.pow(x, y))(const)
  }

  def min (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => math.min(x, y))(const)
  }

  def max (const: Double): Pixel = {
    genericArithmeticOperation((x, y) => math.max(x, y))(const)
  }

  def log (): Pixel = {
    genericFunction(x => math.log(x))()
  }

  def abs (): Pixel = {
    genericFunction(x => math.abs(x))()
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
