package picture

import utility.{HW, Point}

import scala.annotation.tailrec

class Picture (val dim: HW) {
  // private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(y * dim.width + x, y * dim.width + x + 1, y * dim.width + x + 2))
  private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(0))
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
    inverseSub(Pixel.VAL_MAX, startPoint, size)
    this
  }

  def grayscale(startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    operationOnEveryPixelNoConst(toGrayscalePixel, startPoint, size)
    this
  }

  def convolution(kernel: Array[Array[Double]], startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {
    val kernelHeight: Int = kernel.length
    val kernelWidth: Int = kernel.head.length
    val newPixels: Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => {
      val originalPixel = pixels(y)(x)
      new Pixel(originalPixel.r, originalPixel.g, originalPixel.b)
    })

    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      var newRed: Double = 0.0
      var newGreen: Double = 0.0
      var newBlue: Double = 0.0
      for (kH <- 0 until kernelHeight;
           kW <- 0 until kernelWidth) {
        val xn: Int = x + kW - 1
        val yn: Int = y + kH - 1
        if (xn >= 0 && xn < dim.width && yn >= 0 && yn < dim.height) {
          val tmpPixel = pixels(yn)(xn)
          newRed += tmpPixel.r * kernel(kH)(kW)
          newGreen += tmpPixel.g * kernel(kH)(kW)
          newBlue += tmpPixel.b * kernel(kH)(kH)
        }
      }
      newPixels(y)(x) = new Pixel(newRed / (kernelHeight * kernelWidth), newGreen / (kernelHeight * kernelWidth), newBlue / (kernelHeight * kernelWidth))
    }
    for (y <- 0 until this.dim.height;
         x <- 0 until this.dim.width) {
      pixels(y)(x) = newPixels(y)(x)
    }
    this
  }

  def median(halfKernelSize: HW, startPoint: Point = new Point(0, 0), size: HW = dim): Picture = {

    def medianUpTo5(five: Array[Double]): Double = {
      def order2(a: Array[Double], i: Int, j: Int): Unit = {
        if (a(i)>a(j)) { val t = a(i); a(i) = a(j); a(j) = t }
      }

      def pairs(a: Array[Double], i: Int, j: Int, k: Int, l: Int): Double = {
        if (a(i)<a(k)) { order2(a,j,k); a(j) }
        else { order2(a,i,l); a(i) }
      }

      if (five.length < 2) return five(0)
      order2(five,0,1)
      if (five.length < 4) return (
        if (five.length==2 || five(2) < five(0)) five(0)
        else if (five(2) > five(1)) five(1)
        else five(2)
        )
      order2(five,2,3)
      if (five.length < 5) pairs(five,0,1,2,3)
      else if (five(0) < five(2)) { order2(five,1,4); pairs(five,1,4,2,3) }
      else { order2(five,3,4); pairs(five,0,1,3,4) }
    }

    @tailrec
    def medianOfMedians(arr: Array[Double]): Double = {
      val medians: Array[Double] = arr.grouped(5).map(medianUpTo5).toArray
      if (medians.length <= 5) medianUpTo5 (medians)
      else medianOfMedians(medians)
    }

    val halfKernelWidth = halfKernelSize.width
    val halfKernelHeight = halfKernelSize.height

    val newPixels: Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => {
      val originalPixel = pixels(y)(x)
      new Pixel(originalPixel.r, originalPixel.g, originalPixel.b)
    })


    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      val isRed: IndexedSeq[Double] =
        for (kH <- y - halfKernelHeight to y + halfKernelHeight if kH >= 0 && kH < this.dim.height;
             kW <- x - halfKernelWidth to x + halfKernelWidth if kW >= 0 && kW < this.dim.height)
        yield pixels(kH)(kW).r

      val isGreen: IndexedSeq[Double] =
        for (kH <- y - halfKernelHeight to y + halfKernelHeight if kH >= 0 && kH < this.dim.height;
             kW <- x - halfKernelWidth to x + halfKernelWidth if kW >= 0 && kW < this.dim.height)
        yield pixels(kH)(kW).g

      val isBlue: IndexedSeq[Double] =
        for (kH <- y - halfKernelHeight to y + halfKernelHeight if kH >= 0 && kH < this.dim.height;
             kW <- x - halfKernelWidth to x + halfKernelWidth if kW >= 0 && kW < this.dim.height)
        yield pixels(kH)(kW).b

      val arrRed: Array[Double] = isRed.toArray
      val arrGreen: Array[Double] = isGreen.toArray
      val arrBlue: Array[Double] = isBlue.toArray

      val newRed = medianOfMedians(arrRed)
      val newGreen = medianOfMedians(arrGreen)
      val newBlue = medianOfMedians(arrBlue)

      newPixels(y)(x) = new Pixel(newRed, newGreen, newBlue)
    }

    for (y <- 0 until this.dim.height;
         x <- 0 until this.dim.width) {
      pixels(y)(x) = newPixels(y)(x)
    }

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
