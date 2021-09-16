package picture

import utility.{HW, Point, Rectangle}

import scala.annotation.tailrec

class Picture (val dim: HW) extends Serializable {
//  private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(y * dim.width + x, y * dim.width + x + 1, y * dim.width + x + 2))
  private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(0))
  val pixels: Array[Array[Pixel]] = init(this.dim)

  def add (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.add, startPoint, size)(const, checkRange)
    this
  }

  def sub (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.sub, startPoint, size)(const, checkRange)
    this
  }

  def inverseSub (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.inverseSub, startPoint, size)(const, checkRange)
    this
  }

  def mul (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.mul, startPoint, size)(const, checkRange)
    this
  }

  def div (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.div, startPoint, size)(const, checkRange)
    this
  }

  def inverseDiv (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.inverseDiv, startPoint, size)(const, checkRange)
    this
  }

  def pow (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.pow, startPoint, size)(const, checkRange)
    this
  }

  def min (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.min, startPoint, size)(const, checkRange)
    this
  }

  def max (const: Double, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelWithConst(Pixel.max, startPoint, size)(const, checkRange)
    this
  }

  def log (startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelNoConst(Pixel.log, startPoint, size, checkRange)
    this
  }

  def abs (startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelNoConst(Pixel.abs, startPoint, size, checkRange)
    this
  }

  def inversion (startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    inverseSub(Pixel.VAL_MAX, startPoint, size, checkRange)
    this
  }

  def grayscale (startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    operationOnEveryPixelNoConst(Pixel.grayscale, startPoint, size, checkRange)
    this
  }

  def convolution (kernel: Array[Array[Double]], startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    val kernelHeight: Int = kernel.length
    val kernelWidth: Int = kernel.head.length
    val newPixels: Array[Array[Pixel]] = tabulatePixelCopies(this)

    for (y <- startPoint.y until startPoint.y + size.height if y >= 0 && y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x >= 0 && x < this.dim.width) {
      @tailrec
      def xIteration(currKernelY: Int, currKernelX: Int, tempColors: Pixel) : Pixel = {
        if (currKernelX == kernelWidth) {
          tempColors
        } else {
          val xn: Int = x + currKernelX - 1
          val yn: Int = y + currKernelY - 1
          if (xn >= 0 && xn < dim.width && yn >= 0 && yn < dim.height) {
            val tmpPixel = pixels(yn)(xn)
            val newRed: Double = tempColors.r + tmpPixel.r * kernel(currKernelY)(currKernelX)
            val newGreen: Double = tempColors.g + tmpPixel.g * kernel(currKernelY)(currKernelX)
            val newBlue: Double = tempColors.b + tmpPixel.b * kernel(currKernelY)(currKernelX)
            xIteration(currKernelY,
              currKernelX + 1,
              new Pixel(newRed, newGreen, newBlue, false))
          } else {
            xIteration(currKernelY,
              currKernelX + 1,
              tempColors)
          }
        }
      }

      @tailrec
      def yIteration(currKernelY: Int, tempColors: Pixel) : Pixel = {
        if (currKernelY == kernelHeight) {
          tempColors
        } else {
          val newTmpColor: Pixel = xIteration(currKernelY, 0, tempColors)
          yIteration(currKernelY + 1, newTmpColor)
        }
      }

      val newColor: Pixel = yIteration(0, new Pixel(0))
      val newRed: Double = newColor.r
      val newGreen: Double = newColor.g
      val newBlue: Double = newColor.b
      newPixels(y)(x) = new Pixel(newRed /* / (kernelHeight * kernelWidth) */,
                                  newGreen /* / (kernelHeight * kernelWidth) */,
                                  newBlue /* / (kernelHeight * kernelWidth) */,
                                  checkRange)
    }
    overwrite(newPixels, pixels)
    this
  }

  def median (halfKernelSize: HW, startPoint: Point = new Point(0, 0), size: HW = dim, checkRange: Boolean = true): Picture = {
    def medianUpTo5(five: Array[Double]): Double = {
      def oneAndOrderedPair(a: Double, smaller: Double, bigger: Double): Double =
        if (bigger < a) bigger
        else if (a < smaller) smaller else a

      def partialOrder(a: Double, b: Double, c: Double, d: Double): (Double, Double, Double, Double) = {
        val (s1, b1) = if (a < b) (a, b) else (b, a)
        val (s2, b2) = if (c < d) (c, d) else (d, c)
        (s1, b1, s2, b2)
      }

      def medianOf4(a: Double, b: Double, c: Double, d: Double): Double = {
        val (s1, b1, s2, b2) = partialOrder(a, b, c, d)
        if (b1 < b2) oneAndOrderedPair(s2, s1, b1)
        else oneAndOrderedPair(s1, s2, b2)
      }

      five match {
        case Array(a) => a
        case Array(a, b) => a min b
        case Array(a, b, c) => {
          if (a < b) oneAndOrderedPair(c, a, b)
          else oneAndOrderedPair(c, b, a)
        }
        case Array(a, b, c, d) => medianOf4(a, b, c, d)
        case Array(a, b, c, d, e) => {
          val (s1, b1, s2, b2) = partialOrder(a, b, c, d)
          if (s1 < s2) medianOf4(e, b1, s2, b2)
          else medianOf4(e, b2, s1, b1)
        }
      }
    }

    @tailrec
    def medianOfMedians(arr: Array[Double]): Double = {
      val medians: Array[Double] = arr.grouped(5).map(medianUpTo5).toArray
      if (medians.length <= 5) medianUpTo5 (medians)
      else medianOfMedians(medians)
    }

    val newPixels: Array[Array[Pixel]] = tabulatePixelCopies(this)

    for (y <- startPoint.y until startPoint.y + size.height if y >= 0 && y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x >= 0 && x < this.dim.width) {

      val isRed: IndexedSeq[Double] = selectChannel(pixel => pixel.r)(new Point(x, y), halfKernelSize)
      val isGreen: IndexedSeq[Double] = selectChannel(pixel => pixel.g)(new Point(x, y), halfKernelSize)
      val isBlue: IndexedSeq[Double] = selectChannel(pixel => pixel.b)(new Point(x, y), halfKernelSize)

      val arrRed: Array[Double] = isRed.toArray
      val arrGreen: Array[Double] = isGreen.toArray
      val arrBlue: Array[Double] = isBlue.toArray

      val newRed = medianOfMedians(arrRed)
      val newGreen = medianOfMedians(arrGreen)
      val newBlue = medianOfMedians(arrBlue)

      newPixels(y)(x) = new Pixel(newRed, newGreen, newBlue, checkRange)
    }

    overwrite(newPixels, pixels)
    this
  }

  def applyColor (startPoint: Point = new Point(0, 0), size: HW = dim, red: Double, green: Double, blue: Double, checkRange: Boolean = true): Picture = {
    for (y <- startPoint.y until startPoint.y + size.height if y >= 0 && y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x >= 0 && x < this.dim.width) {
      pixels(y)(x) = Pixel.applyColor(pixels(y)(x), red, green, blue, checkRange)
    }
    this
  }

  def extract(rect: Rectangle): Picture = {
    val extractedPicture: Picture = new Picture(rect.dim)
    for (j <- 0 until rect.dim.height;
         i <- 0 until rect.dim.width) {
      if (rect.topLeftCorner.y + j >= 0 && rect.topLeftCorner.y + j < this.dim.height &&
        rect.topLeftCorner.x + i >= 0 && rect.topLeftCorner.x + i < this.dim.width)
        extractedPicture.pixels(j)(i) = pixels(rect.topLeftCorner.y + j)(rect.topLeftCorner.x + i)
    }
    extractedPicture
  }

  /**
   * Returns the array of (r, g or b) channels.
   * Point is center of the rectangle.
   * Half-kernel is number of pixels NESW of center pixel.
   */
  private def selectChannel(channel: Pixel => Double)(point: Point, halfKernel: HW): IndexedSeq[Double] = {
    val channels: IndexedSeq[Double] =
      for (kH <- point.y - halfKernel.height to point.y + halfKernel.height if kH >= 0 && kH < this.dim.height;
           kW <- point.x - halfKernel.width to point.x + halfKernel.width if kW >= 0 && kW < this.dim.width)
      yield channel(pixels(kH)(kW))
    channels
  }

  /**
   * Returns copies of pixels.
   */
  private def tabulatePixelCopies(picture: Picture): Array[Array[Pixel]] = {
    val newPixels: Array[Array[Pixel]] = Array.tabulate(picture.dim.height, picture.dim.width)((y, x) => {
      val originalPixel = picture.pixels(y)(x)
      new Pixel(originalPixel.r, originalPixel.g, originalPixel.b)
    })
    newPixels
  }

  private def operationOnEveryPixelWithConst (operation: (Pixel, Double, Boolean) => Pixel, startPoint: Point, size: HW)(const: Double, checkRange: Boolean = true): Unit = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = operation(pixels(y)(x), const, checkRange)
    }
  }

  private def operationOnEveryPixelNoConst (operation: (Pixel, Boolean) => Pixel, startPoint: Point, size: HW, checkRange: Boolean = true): Unit = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = operation(pixels(y)(x), checkRange)
    }
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

  private def overwrite(from: Array[Array[Pixel]], to: Array[Array[Pixel]]): Unit = {
    for (y <- 0 until this.dim.height;
         x <- 0 until this.dim.width) {
      to(y)(x) = from(y)(x)
    }
  }
}
