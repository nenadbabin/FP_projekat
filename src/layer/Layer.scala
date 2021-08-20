package layer

import picture.Picture
import utility.{HW, Point}

class Layer (val picture: Picture,
             val name: String,
             var transparency: Double = 1,
             var active: Boolean = true) {

  def toGrayscale(startPoint: Point, size: HW): Unit = {
    picture.toGrayscale(startPoint, size)
  }

  def applyColor(startPoint: Point, size: HW, red: Double, green: Double, blue: Double): Unit = {
    if (active) picture.applyColor(startPoint, size, red, green, blue)
  }
}
