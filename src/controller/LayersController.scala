package controller

import javafx.scene.canvas.Canvas
import javafx.scene.image.PixelWriter
import javafx.scene.layout.{Pane, StackPane}
import javafx.scene.paint.Color
import layer.Layer
import picture.Pixel
import utility.HW

import scala.annotation.tailrec
import scala.util.matching.Regex

class LayersController {

  var layers: List[Layer] = List()

  def activeLayers: List[Layer] = layers.filter((layer: Layer) => layer.active)
  def layersNames: List[String] = for (layer <- layers.reverse) yield layer.name

  def add_layer(new_layer: Layer): Unit = {
    def add_layer_to_list(new_layer: Layer): List[Layer] = layers match {
      case List() => List(new_layer)
      case _ => new_layer :: layers
    }

    layers = add_layer_to_list(new_layer)
  }

  def drawLayers(): Pane = {
    val dimsList: List[HW] = for (layer <- activeLayers.reverse) yield layer.picture.dim

    val maxDim = this.findMaxDimension(dimsList)

    val pane: Pane = new StackPane()

    for (layer <- layers.reverse) {
      val picture = layer.picture
      val transparency = layer.transparency
      val active = layer.active

      if (active) {
        val layerCanvas = new Canvas()
        layerCanvas.setWidth(maxDim.width)
        layerCanvas.setHeight(maxDim.height)
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
  }

  def findLayerByName(name: String): Option[Layer] = {
    @tailrec
    def find(list: List[Layer]): Option[Layer] = list match {
      case List() => None
      case List(elem, _*) if elem.name == name => Some(elem)
      case h :: t => find(t)
    }
    find(layers)
  }

  private def changeLayer(layerOp: (Layer, AnyVal) => Layer)(name: String, feature: AnyVal): List[Layer] = {
    @tailrec
    def change(list: List[Layer], newList: List[Layer]): List[Layer] = list match {
      case List() => newList
      case h :: t if h.name == name => {
        val newElem: Layer = layerOp(h, feature)
        val list: List[Layer] = newList :+ newElem
        change(t, list)
      }
      case h :: t => change(t, newList :+ h)
    }
    change(layers, List[Layer]())
  }

  def changeLayerTransparency(name: String, transparency: Double): Unit = {
    layers = changeLayer((h, tr) => h.copy(transparency = tr.asInstanceOf[Double]))(name, transparency)
  }

  def changeLayerActive(name: String, active: Boolean): Unit = {
    layers = changeLayer((h, ac) => h.copy(active = ac.asInstanceOf[Boolean]))(name, active)
  }

  def countLayersWithSimilarNames(name: String): Int = {
    val pattern: Regex = ("""^""" + name + """.*$""").r
    @tailrec
    def find(count: Int, list: List[Layer]): Int = list match {
      case List() => count
      case h::t => {
        val found: Int = h.name match {
          case pattern() => 1
          case _ => 0
        }
        val newCount = count + found
        find(newCount, t)
      }
    }
    find(0, layers)
  }

  def findMaxDimension(list: List[HW]): HW = {
    @tailrec
    def findDim(list: List[HW], maxDim: HW): HW = list match {
      case List() => maxDim
      case h :: t if h.height > maxDim.height && h.width > maxDim.width => findDim(t, new HW(h.height, h.width))
      case h :: t if h.height > maxDim.height && h.width <= maxDim.width => findDim(t, new HW(h.height, maxDim.width))
      case h :: t if h.height <= maxDim.height && h.width > maxDim.width => findDim(t, new HW(maxDim.height, h.width))
      case h :: t => findDim(t, maxDim)
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
