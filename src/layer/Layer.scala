package layer

import picture.Picture
import utility.{HW, Point}

class Layer (val picture: Picture,
             val name: String,
             var transparency: Double = 1,
             var active: Boolean = true) {

  def grayscale(startPoint: Point, size: HW): Unit = {
    picture.grayscale(startPoint, size)
  }

  def sobel(startPoint: Point, size: HW): Unit = {
    val kernel = Array[Array[Double]](
      Array[Double](-1, -1, -1),
      Array[Double](-1, 8, -1),
      Array[Double](-1, -1, -1),
    )
    picture.convolution(kernel, startPoint, size)
  }

  def median(startPoint: Point, size: HW): Unit = {
    picture.median(new HW(3, 3), startPoint, size)
  }

  def inversion(startPoint: Point, size: HW): Unit = {
    picture.inversion(startPoint, size)
  }

  def add(const: Double, startPoint: Point, size: HW): Unit = {
    picture.add(const, startPoint, size)
  }

  def sub(const: Double, startPoint: Point, size: HW): Unit = {
    picture.sub(const, startPoint, size)
  }

  def inverseSub(const: Double, startPoint: Point, size: HW): Unit = {
    picture.inverseSub(const, startPoint, size)
  }

  def mul(const: Double, startPoint: Point, size: HW): Unit = {
    picture.mul(const, startPoint, size)
  }

  def div(const: Double, startPoint: Point, size: HW): Unit = {
    picture.div(const, startPoint, size)
  }

  def inverseDiv(const: Double, startPoint: Point, size: HW): Unit = {
    picture.inverseDiv(const, startPoint, size)
  }

  def pow(const: Double, startPoint: Point, size: HW): Unit = {
    picture.pow(const, startPoint, size)
  }

  def min(const: Double, startPoint: Point, size: HW): Unit = {
    picture.min(const, startPoint, size)
  }

  def max(const: Double, startPoint: Point, size: HW): Unit = {
    picture.max(const, startPoint, size)
  }

  def log(startPoint: Point, size: HW): Unit = {
    picture.log(startPoint, size)
  }

  def abs(startPoint: Point, size: HW): Unit = {
    picture.abs(startPoint, size)
  }

  def applyColor(startPoint: Point, size: HW, red: Double, green: Double, blue: Double): Unit = {
    if (active) picture.applyColor(startPoint, size, red, green, blue)
  }
}
