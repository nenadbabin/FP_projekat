package selection

import layer.Layer
import utility.{HW, Point, Rectangle}

case class Selection (override val name: String,
                      rectangles: List[Rectangle]) extends BaseSelection(name) {

  override protected def genericNoConst(foo: (Layer, Point, HW) => Unit)
                                       (layers: List[Layer]): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      foo(layer, rect.topLeftCorner, rect.dim)
    }
  }

  override protected def genericWithConst(foo: (Layer, Double, Point, HW) => Unit)
                                         (const: Double, layers: List[Layer]): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      foo(layer, const, rect.topLeftCorner, rect.dim)
    }
  }

  override protected def genericWithColor(foo: (Layer, Double, Double, Double, Point, HW) => Unit)
                                         (red: Double, green: Double, blue: Double, layers: List[Layer]): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      foo(layer, red, green, blue, rect.topLeftCorner, rect.dim)
    }
  }
}
