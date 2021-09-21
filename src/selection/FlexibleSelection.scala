package selection
import layer.Layer
import utility.{HW, Point, Rectangle}

class FlexibleSelection (name: String) extends BaseSelection (name) {
  override protected def genericNoConst(foo: (Layer, Point, HW) => Unit)
                                       (layers: List[Layer]): Unit = {
    val topLeftCorner: Point = new Point(0, 0)
    for (layer <- layers) {
      val layerDims: HW = layer.picture.dim
      addToBackups(layer, new Rectangle(topLeftCorner,  layerDims))
      foo(layer, topLeftCorner, layerDims)
    }
  }

  override protected def genericWithConst(foo: (Layer, Double, Point, HW) => Unit)
                                         (const: Double, layers: List[Layer]): Unit = {
    val topLeftCorner: Point = new Point(0, 0)
    for (layer <- layers) {
      val layerDims: HW = layer.picture.dim
      addToBackups(layer, new Rectangle(topLeftCorner,  layerDims))
      foo(layer, const, topLeftCorner, layerDims)
    }
  }

  override protected def genericWithColor(foo: (Layer, Double, Double, Double, Point, HW) => Unit)
                                         (red: Double, green: Double, blue: Double, layers: List[Layer]): Unit = {
    val topLeftCorner: Point = new Point(0, 0)
    for (layer <- layers) {
      val layerDims: HW = layer.picture.dim
      addToBackups(layer, new Rectangle(topLeftCorner,  layerDims))
      foo(layer, red, green, blue, topLeftCorner, layerDims)
    }
  }
}
