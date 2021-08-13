package controller

import javafx.scene.canvas.Canvas
import javafx.scene.image.PixelWriter
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import layer.Layer
import picture.Pixel
import utility.HW

import scala.annotation.tailrec

class HomeController {

  var layers: List[Layer] = List()

  def add_layer(new_layer: Layer): Unit = {
    def add_layer_to_list(new_layer: Layer): List[Layer] = layers match {
      case List() => List(new_layer)
      case _ => List[Layer](new_layer) ::: layers
    }

    layers = add_layer_to_list(new_layer)
  }

  def draw(): Pane = {
    val dims_list: List[HW] = for (layer <- layers.reverse) yield layer.picture.dim

    val max_dim = this.findMaxDimension(dims_list)

    val pane: Pane = new Pane()

    for (layer <- layers.reverse) {
      println(layer.name)
      val picture = layer.picture
      val transparency = layer.transparency
      val active = layer.active

      if (active) {
        val layerCanvas = new Canvas()
        layerCanvas.setWidth(max_dim.width)
        layerCanvas.setHeight(max_dim.height)
        val pixelWriter: PixelWriter = layerCanvas.getGraphicsContext2D.getPixelWriter

        for (y <- 0 until picture.dim.height;
             x <- 0 until picture.dim.width) {
          val pixel: Pixel = picture.pixels(y)(x)
          val r = pixel.r
          val g = pixel.g
          val b = pixel.b
          pixelWriter.setColor(x, y, new Color(r, g, b, transparency))
        }

        pane.getChildren.add(layerCanvas)
      }
    }

    pane
//    val rect1: Rectangle = new Rectangle(new Point(0,0), new HW(256,256))
//    val rect2: Rectangle = new Rectangle(new Point(256,256), new HW(256,256))
//    val rect_list: List[Rectangle] = List[Rectangle](rect1, rect2)
//    val selection: Selection = new Selection("sel_1", rect_list)

//    for (rect <- selection.rectangles) {
//      picture.toGrayscale(rect.topLeftCorner, rect.dim)
//    }

  }

  def findLayerByName(name: String): Layer = {
    def find(list: List[Layer]): Layer = list match {
      case List() => null
      case h::t =>
        if (h.name == name) h
        else find(t)
    }
    find(layers)
  }

  private def findMaxDimension(list: List[HW]) = {
    @tailrec
    def findDim(list: List[HW], maxDim: HW): HW = list match {
      case List() => maxDim
      case h :: t =>
        if (h.height > maxDim.height && h.width > maxDim.width) findDim(list.tail, new HW(h.height, h.width))
        else if (h.height > maxDim.height && h.width <= maxDim.width) findDim(t, new HW(h.height, maxDim.width))
        else if (h.height <= maxDim.height && h.width > maxDim.width) findDim(t, new HW(maxDim.height, h.width))
        else findDim(t, maxDim)
    }

    findDim(list, new HW(0, 0))
  }

  def moveLayerBackwards(layer: Layer): Unit = {
    @tailrec
    def swapLayers(list: List[Layer], newList: List[Layer], elem: Layer): List[Layer] = list match {
      case List() => newList
      case h :: t =>
        if (h.name == layer.name) {
          if (t != Nil) swapLayers(t, newList, h) // Save elem and don't append
          else newList ::: List[Layer](h) // Layer is already at the bottom, just append and return
        }
        else {
          // Return elems[1..x-1] ::: elem[x+1] ::: elem[x] ::: elems[x+2..N]
          // This branch is called after the x element is found (current is x+1)
          if (elem != null) newList ::: List[Layer](h) ::: List[Layer](elem) ::: t
          else swapLayers(t, newList ::: List[Layer](h), null) // Continue appending
        }
    }

    this.layers = swapLayers(this.layers, List[Layer](), null)
  }

  def moveLayerForwards(layer: Layer): Unit = {
    this.layers = this.layers.reverse
    moveLayerBackwards(layer)
    this.layers = this.layers.reverse
  }
}
