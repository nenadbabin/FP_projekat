package selection

import layer.Layer
import picture.{Picture, Pixel}
import utility.{HW, Point, Rectangle}

import scala.collection.mutable

case class Selection (val name: String,
                      val rectangles: List[Rectangle]) {

  private val backups: mutable.HashMap[Layer, List[Backup]] = new mutable.HashMap[Layer, List[Backup]]()

  private def addToBackups(layer: Layer, rect: Rectangle): Unit = {
    val extractedPicture: Picture = layer.extract(rect)
    val backup: Backup = new Backup(rect.topLeftCorner, rect.dim, extractedPicture)
    if (backups.contains(layer)) {
      val current: List[Backup] = backups(layer)
      backups.addOne((layer,  backup :: current))
    } else {
      backups.addOne((layer, List(backup)))
    }
  }

  def deleteSelection(): Unit = {
    for ((layer: Layer, layerBackups: List[Backup]) <- backups) {
      for (backup <- layerBackups) {
        layer.restore(new Rectangle(backup.position, backup.dim), backup.pictureExtract)
      }
    }
  }

  def applyColor(layers: List[Layer], red: Double, green: Double, blue: Double): Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.applyColor(rect.topLeftCorner, rect.dim, red, green, blue)
    }
  }

  def grayscale(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.grayscale(rect.topLeftCorner, rect.dim)
    }
  }

  def sobel(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.sobel(rect.topLeftCorner, rect.dim)
    }
  }

  def median(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.median(rect.topLeftCorner, rect.dim)
    }
  }

  def inversion(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.inversion(rect.topLeftCorner, rect.dim)
    }
  }

  def add(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.add(const, rect.topLeftCorner, rect.dim)
    }
  }

  def sub(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.sub(const, rect.topLeftCorner, rect.dim)
    }
  }

  def inverseSub(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.inverseSub(const, rect.topLeftCorner, rect.dim)
    }
  }

  def mul(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.mul(const, rect.topLeftCorner, rect.dim)
    }
  }

  def div(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.div(const, rect.topLeftCorner, rect.dim)
    }
  }

  def inverseDiv(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.inverseDiv(const, rect.topLeftCorner, rect.dim)
    }
  }

  def pow(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.pow(const, rect.topLeftCorner, rect.dim)
    }
  }

  def min(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.min(const, rect.topLeftCorner, rect.dim)
    }
  }

  def max(const: Double, layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.max(const, rect.topLeftCorner, rect.dim)
    }
  }

  def log(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.log(rect.topLeftCorner, rect.dim)
    }
  }

  def abs(layers: List[Layer]) : Unit = {
    for (layer <- layers;
         rect <- rectangles) {
      addToBackups(layer, rect)
      layer.abs(rect.topLeftCorner, rect.dim)
    }
  }

}
