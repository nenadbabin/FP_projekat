package layer

import picture.Picture
import utility.{HW, Point, Rectangle}

class Layer (val picture: Picture,
             val name: String,
             var transparency: Double = 1,
             var active: Boolean = true) extends Serializable {

  def grayscale(startPoint: Point, size: HW = picture.dim): Unit = {
    picture.grayscale(startPoint, size)
  }

  def sobel(startPoint: Point, size: HW = picture.dim): Unit = {
    val kernel = Array[Array[Double]](
      Array[Double](-1, -1, -1),
      Array[Double](-1, 8, -1),
      Array[Double](-1, -1, -1),
    )
    picture.convolution(kernel, startPoint, size)
  }

  def median(startPoint: Point, size: HW = picture.dim): Unit = {
    picture.median(new HW(3, 3), startPoint, size)
  }

  def inversion(startPoint: Point, size: HW = picture.dim): Unit = {
    picture.inversion(startPoint, size)
  }

  def add(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.add(const, startPoint, size)
  }

  def sub(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.sub(const, startPoint, size)
  }

  def inverseSub(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.inverseSub(const, startPoint, size)
  }

  def mul(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.mul(const, startPoint, size)
  }

  def div(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.div(const, startPoint, size)
  }

  def inverseDiv(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.inverseDiv(const, startPoint, size)
  }

  def pow(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.pow(const, startPoint, size)
  }

  def min(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.min(const, startPoint, size)
  }

  def max(const: Double, startPoint: Point, size: HW = picture.dim): Unit = {
    picture.max(const, startPoint, size)
  }

  def log(startPoint: Point, size: HW = picture.dim): Unit = {
    picture.log(startPoint, size)
  }

  def abs(startPoint: Point, size: HW = picture.dim): Unit = {
    picture.abs(startPoint, size)
  }

  def applyColor(startPoint: Point, size: HW = picture.dim, red: Double, green: Double, blue: Double): Unit = {
    picture.applyColor(startPoint, size, red, green, blue)
  }

  def extract(rect: Rectangle): Picture = {
    val extractedPicture: Picture = new Picture(rect.dim)
    for (j <- 0 until rect.dim.height;
         i <- 0 until rect.dim.width) {
      if (rect.topLeftCorner.y + j >= 0 && rect.topLeftCorner.y + j < picture.dim.height &&
          rect.topLeftCorner.x + i >= 0 && rect.topLeftCorner.x + i < picture.dim.width)
        extractedPicture.pixels(j)(i) = picture.pixels(rect.topLeftCorner.y + j)(rect.topLeftCorner.x + i)
    }
    extractedPicture
  }

  def restore(rect: Rectangle, backup: Picture): Unit = {
    for (j <- 0 until rect.dim.height;
         i <- 0 until rect.dim.width) {
      if (rect.topLeftCorner.y + j >= 0 && rect.topLeftCorner.y + j < picture.dim.height &&
        rect.topLeftCorner.x + i >= 0 && rect.topLeftCorner.x + i < picture.dim.width)
        picture.pixels(rect.topLeftCorner.y + j)(rect.topLeftCorner.x + i) = backup.pixels(j)(i)
    }
  }
}
