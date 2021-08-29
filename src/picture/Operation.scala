package picture

import utility.{HW, Point}

abstract class Operation(val startPoint: Point,
                         val size: HW,
                         val next: Operation = null) {
  def apply(picture: Picture): Picture
  protected def performPixelValuesCheck(op: Operation): Boolean = if (op == null) true else false
}

class Add (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.add(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Sub (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.sub(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class InverseSub (val const: Double,
                  startPoint: Point,
                  size: HW,
                  next: Operation = null) extends Operation (startPoint, size, null) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inverseSub(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Mul (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.mul(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Div (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.div(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class InverseDiv (val const: Double,
                  startPoint: Point,
                  size: HW,
                  next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inverseDiv(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Pow (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.pow(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Min (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.min(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Max (val const: Double,
           startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.max(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Log (startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.log(startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Abs (startPoint: Point,
           size: HW,
           next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.abs(startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Inversion (startPoint: Point,
                 size: HW,
                 next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inversion(startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class Grayscale (startPoint: Point,
                 size: HW,
                 next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.grayscale(startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}
