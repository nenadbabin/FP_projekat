package selection

import layer.Layer
import picture.Picture
import utility.{HW, Point, Rectangle}

import scala.collection.mutable

abstract class BaseSelection (val name: String) {

  protected def genericNoConst(foo: (Layer, Point, HW) => Unit)
                              (layers: List[Layer])
  protected def genericWithConst(foo: (Layer, Double, Point, HW) => Unit)
                                (const: Double, layers: List[Layer]): Unit
  protected def genericWithColor(foo: (Layer, Double, Double, Double, Point, HW) => Unit)
                                (red: Double, green: Double, blue: Double, layers: List[Layer]): Unit
  protected val backups: mutable.HashMap[Layer, List[Backup]] = new mutable.HashMap[Layer, List[Backup]]()

  def grayscale(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.grayscale(topLeftCorner, dim)})(layers)
  }

  def sobel(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.sobel(topLeftCorner, dim)})(layers)
  }

  def median(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.median(topLeftCorner, dim)})(layers)
  }

  def inversion(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.inversion(topLeftCorner, dim)})(layers)
  }

  def add(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.add(const, topLeftCorner, dim)})(const, layers)
  }

  def sub(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.sub(const, topLeftCorner, dim)})(const, layers)
  }

  def inverseSub(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.inverseSub(const, topLeftCorner, dim)})(const, layers)
  }

  def mul(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.mul(const, topLeftCorner, dim)})(const, layers)
  }

  def div(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.div(const, topLeftCorner, dim)})(const, layers)
  }

  def inverseDiv(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.inverseDiv(const, topLeftCorner, dim)})(const, layers)
  }

  def pow(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.pow(const, topLeftCorner, dim)})(const, layers)
  }

  def min(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.min(const, topLeftCorner, dim)})(const, layers)
  }

  def max(const: Double, layers: List[Layer]): Unit = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.max(const, topLeftCorner, dim)})(const, layers)
  }

  def log(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) =>
    {layer.log(topLeftCorner, dim)})(layers)
  }

  def abs(layers: List[Layer]): Unit = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) =>
    {layer.abs(topLeftCorner, dim)})(layers)
  }

  def applyColor(layers: List[Layer], red: Double, green: Double, blue: Double): Unit = {
    genericWithColor((layer: Layer, red: Double, green: Double, blue: Double, topLeftCorner: Point, dim: HW) =>
    {layer.applyColor(topLeftCorner, dim, red, green, blue)})(red, green, blue, layers)
  }

  def restore(): Unit = {
    for ((layer: Layer, layerBackups: List[Backup]) <- backups) {
      for (backup <- layerBackups) {
        layer.restore(new Rectangle(backup.position, backup.dim), backup.pictureExtract)
      }
    }
    backups.clear()
  }

  protected def addToBackups(layer: Layer, rect: Rectangle): Unit = {
    val extractedPicture: Picture = layer.extract(rect)
    val backup: Backup = new Backup(rect.topLeftCorner, rect.dim, extractedPicture)
    if (backups.contains(layer)) {
      val current: List[Backup] = backups(layer)
      backups.addOne((layer,  backup :: current))
    } else {
      backups.addOne((layer, List(backup)))
    }
  }
}
