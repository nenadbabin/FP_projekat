package picture

import utility.{HW, Point}

import scala.annotation.tailrec

class Picture (val dim: HW) {
  private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(y * dim.width + x, y * dim.width + x + 1, y * dim.width + x + 2))
  val pixels: Array[Array[Pixel]] = init(dim)

  def add(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(addPixel, startPoint, size)(const)
    this
  }

  def sub(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(subPixel, startPoint, size)(const)
    this
  }

  def inverseSub(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(inverseSubPixel, startPoint, size)(const)
    this
  }

  def mul(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(mulPixel, startPoint, size)(const)
    this
  }

  def div(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(divPixel, startPoint, size)(const)
    this
  }

  def inverseDiv(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(inverseDivPixel, startPoint, size)(const)
    this
  }

  def pow(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(powPixel, startPoint, size)(const)
    this
  }

  def min(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(minPixel, startPoint, size)(const)
    this
  }

  def max(const: Double, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelWithConst(maxPixel, startPoint, size)(const)
    this
  }

  def log(startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelNoConst(logPixel, startPoint, size)
    this
  }

  def abs(startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelNoConst(absPixel, startPoint, size)
    this
  }

  def inversion(startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    inverseSub(1, startPoint, size)
    this
  }

  def grayscale(startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelNoConst(toGrayscalePixel, startPoint, size)
    this
  }

  def applyColor(startPoint: Point = new Point(0, 0), size: HW = dim, red: Double, green: Double, blue: Double): Picture = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = new Pixel(red, green, blue)
    }
    this
  }

  private def operationOnEveryPixelWithConst (operation: (Pixel, Double) => Pixel, startPoint: Point, size: HW)(const: Double): Unit = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = operation(pixels(y)(x), const)
    }
  }

  private def operationOnEveryPixelNoConst (operation: Pixel => Pixel, startPoint: Point, size: HW): Unit = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = operation(pixels(y)(x))
    }
  }

  private def genericArithmeticOperationWithConst (operation: (Double, Double) => Double)(pixel: Pixel, const: Double): Pixel = {
    def genericInner (const: Double): Pixel = {
      val newR = operation(pixel.r, const)
      val newG = operation(pixel.g, const)
      val newB = operation(pixel.b, const)

      new Pixel(newR, newG, newB)
    }
    genericInner(const)
  }

  private def genericFunctionWithConst(operation: (Double, Double) => Double)(pixel: Pixel, const: Double): Pixel =
    genericArithmeticOperationWithConst(operation)(pixel, const)

  private def genericFunctionNoConst (operation: Double => Double)(pixel: Pixel): Pixel = {
    def genericInner: Pixel = {
      val newR = operation(pixel.r)
      val newG = operation(pixel.g)
      val newB = operation(pixel.b)

      new Pixel(newR, newG, newB)
    }
    genericInner
  }

  private def addPixel (pixel: Pixel, const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x + y)(pixel, const)
  }

  private def subPixel (pixel: Pixel, const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x - y)(pixel, const)
  }

  private def inverseSubPixel (pixel: Pixel, const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => y - x)(pixel, const)
  }

  private def mulPixel (pixel: Pixel, const: Double): Pixel = {
    genericArithmeticOperationWithConst((x, y) => x * y)(pixel, const)
  }

  private def divPixel (pixel: Pixel, const: Double): Pixel = {
    if (const == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => x / y)(pixel, const)
  }

  private def inverseDivPixel (pixel: Pixel, const: Double): Pixel = {
    if (pixel.r == 0 || pixel.g == 0 || pixel.b == 0) throw new ArithmeticException("Division by 0.")
    genericArithmeticOperationWithConst((x, y) => y / x)(pixel, const)
  }

  private def powPixel (pixel: Pixel, const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.pow(x, y))(pixel, const)
  }

  private def minPixel (pixel: Pixel, const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.min(x, y))(pixel, const)
  }

  private def maxPixel (pixel: Pixel, const: Double): Pixel = {
    genericFunctionWithConst((x, y) => math.max(x, y))(pixel, const)
  }

  private def logPixel (pixel: Pixel): Pixel = {
    genericFunctionNoConst(x => math.log(x))(pixel)
  }

  private def absPixel (pixel: Pixel): Pixel = {
    genericFunctionNoConst(x => math.abs(x))(pixel)
  }

  private def toGrayscalePixel (pixel: Pixel): Pixel = {
    val avg = (pixel.r + pixel.g + pixel.b) / 3
    new Pixel(avg, avg, avg)
  }

  override def toString: String = {
    @tailrec
    def printRow(currString: String, currRow: Int, currPixel: Int): String = {
      if (currPixel == this.dim.width) {
        currString
      } else {
        val tmpPixel = this.pixels(currRow)(currPixel)
        val newString = f"${currString} ${tmpPixel.r}%2.2f|${tmpPixel.g}%2.2f|${tmpPixel.b}%2.2f ||"
        printRow(newString, currRow, currPixel + 1)
      }
    }

    @tailrec
    def printColumns(currString: String, currRow: Int): String = {
      if (currRow == this.dim.height) {
        currString
      } else {
        val newString = s"${printRow(currString, currRow, 0)}\n"
        printColumns(newString, currRow + 1)
      }
    }

    printColumns("", 0)
  }

}
