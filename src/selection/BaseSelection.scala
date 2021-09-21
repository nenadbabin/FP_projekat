package selection

import layer.Layer
import picture.Picture
import utility.{HW, Point, Rectangle}

import scala.collection.mutable

abstract class BaseSelection (val name: String) extends Serializable {

  protected def genericNoConst(foo: (Layer, Point, HW) => Unit)
                              (layers: List[Layer])
  protected def genericWithConst(foo: (Layer, Double, Point, HW) => Unit)
                                (const: Double, layers: List[Layer]): Unit
  protected def genericWithColor(foo: (Layer, Double, Double, Double, Point, HW) => Unit)
                                (red: Double, green: Double, blue: Double, layers: List[Layer]): Unit
  protected val backups: mutable.HashMap[String, List[Backup]] = new mutable.HashMap[String, List[Backup]]()

  def grayscale(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.grayscale(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def sobel(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.sobel(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def sharpen(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.sharpen(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def gaussianBlur(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.gaussianBlur(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def median(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.median(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def inversion(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) => {layer.inversion(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def add(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.add(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def sub(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.sub(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def inverseSub(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.inverseSub(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def mul(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.mul(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def div(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    if (const == 0) return (false, Some("Division by 0."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.div(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def inverseDiv(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    try {
      genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
      {layer.inverseDiv(const, topLeftCorner, dim)})(const, layers)
      (true, None)
    } catch {
      case e: ArithmeticException => (false, Some(e.getMessage))
    }
  }

  def pow(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.pow(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def min(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.min(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def max(const: Double, layers: List[Layer]): (Boolean, Option[String]) = {
    if (const < 0 || const > 1) return (false, Some("Const should be in range of 0 and 1."))
    genericWithConst((layer: Layer, const: Double, topLeftCorner: Point, dim: HW) =>
    {layer.max(const, topLeftCorner, dim)})(const, layers)
    (true, None)
  }

  def log(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) =>
    {layer.log(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def abs(layers: List[Layer]): (Boolean, Option[String]) = {
    genericNoConst((layer: Layer, topLeftCorner: Point, dim: HW) =>
    {layer.abs(topLeftCorner, dim)})(layers)
    (true, None)
  }

  def applyColor(layers: List[Layer], red: Double, green: Double, blue: Double): (Boolean, Option[String]) = {
    genericWithColor((layer: Layer, red: Double, green: Double, blue: Double, topLeftCorner: Point, dim: HW) =>
    {layer.applyColor(topLeftCorner, dim, red, green, blue)})(red, green, blue, layers)
    (true, None)
  }

  def restore(layers: List[Layer]): Unit = {
    for ((layerName: String, layerBackups: List[Backup]) <- backups) {
      for (backup <- layerBackups) {
        val layer: Layer = layers.filter(l => l.name == layerName).head
        layer.restore(new Rectangle(backup.position, backup.dim), backup.pictureExtract)
      }
    }
    // Clearing backups is needed for FlexibleSelection.
    // Selection is deleted after calling restore, so clear is not needed.
    backups.clear()
  }

  protected def addToBackups(layer: Layer, rect: Rectangle): Unit = {
    val extractedPicture: Picture = layer.extract(rect)
    val backup: Backup = new Backup(rect.topLeftCorner, rect.dim, extractedPicture)
    if (backups.contains(layer.name)) {
      val current: List[Backup] = backups(layer.name)
      backups.addOne((layer.name,  backup :: current))
    } else {
      backups.addOne((layer.name, List(backup)))
    }
  }
}
