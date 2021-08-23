package selection

import layer.Layer
import picture.Pixel
import utility.{HW, Point, Rectangle}

case class Selection (val name: String,
                      val rectangles: List[Rectangle]) {

  def applyColor(layers: List[Layer], red: Double, green: Double, blue: Double): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.applyColor(rect.topLeftCorner, rect.dim, red, green, blue)
    }
  }

  def grayscale(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.grayscale(rect.topLeftCorner, rect.dim)
    }
  }

  def sobel(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.sobel(rect.topLeftCorner, rect.dim)
    }
  }

  def median(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.median(rect.topLeftCorner, rect.dim)
    }
  }

  def inversion(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.inversion(rect.topLeftCorner, rect.dim)
    }
  }

  def add(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.add(const, rect.topLeftCorner, rect.dim)
    }
  }

  def sub(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.sub(const, rect.topLeftCorner, rect.dim)
    }
  }

  def inverseSub(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.inverseSub(const, rect.topLeftCorner, rect.dim)
    }
  }

  def mul(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.mul(const, rect.topLeftCorner, rect.dim)
    }
  }

  def div(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.div(const, rect.topLeftCorner, rect.dim)
    }
  }

  def inverseDiv(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.inverseDiv(const, rect.topLeftCorner, rect.dim)
    }
  }

  def pow(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.pow(const, rect.topLeftCorner, rect.dim)
    }
  }

  def min(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.min(const, rect.topLeftCorner, rect.dim)
    }
  }

  def max(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.max(const, rect.topLeftCorner, rect.dim)
    }
  }

  def log(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.log(rect.topLeftCorner, rect.dim)
    }
  }

  def abs(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.abs(rect.topLeftCorner, rect.dim)
    }
  }

}
