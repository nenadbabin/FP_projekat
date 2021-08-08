package picture

import utility.{HW, Point}

import scala.annotation.tailrec

class Picture (val dim: HW) {
  private def init (dim: HW): Array[Array[Pixel]] = Array.tabulate(dim.height, dim.width)((y, x) => new Pixel(y * dim.width + x, y * dim.width + x + 1, y * dim.width + x + 2))
  val pixels: Array[Array[Pixel]] = init(dim)

  def toGrayscale(startPoint: Point = new Point(0, 0), size: HW = dim): Unit = {
    for (y <- startPoint.y until startPoint.y + size.height if y < this.dim.height;
         x <- startPoint.x until startPoint.x + size.width if x < this.dim.width) {
      pixels(y)(x) = pixels(y)(x).toGrayscale()
    }
  }

  def median(center: Point, neighbors: HW): Unit = {

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
