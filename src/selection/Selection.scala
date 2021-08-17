package selection

import layer.Layer
import utility.Rectangle

case class Selection (val name: String,
                      val rectangles: List[Rectangle]) {

  def applyColor(layers: List[Layer], red: Double, green: Double, blue: Double): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      layer.applyColor(rect.topLeftCorner, rect.dim, red, green, blue)
    }
  }
}
