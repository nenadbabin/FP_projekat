package picture

import utility.{HW, Point}

abstract class Operation(val startPoint: Point,
                         val size: HW,
                         val next: Option[Operation] = None) {
  def apply(picture: Picture): Picture

  protected def performPixelValuesCheck(op: Option[Operation]): Boolean = op match {
    case Some(_) => true
    case None => false
  }

  protected def callNextIfExistsOrReturnResult(result: Picture, next: Option[Operation]): Picture = next match {
    case Some(op) => op.apply(result)
    case None => result
  }
}

class Add (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.add(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Sub (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.sub(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class InverseSub (val const: Double,
                  startPoint: Point,
                  size: HW,
                  next: Option[Operation] = None) extends Operation (startPoint, size, null) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inverseSub(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Mul (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.mul(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Div (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.div(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class InverseDiv (val const: Double,
                  startPoint: Point,
                  size: HW,
                  next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inverseDiv(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Pow (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.pow(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Min (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.min(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Max (val const: Double,
           startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.max(const, startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Log (startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.log(startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Abs (startPoint: Point,
           size: HW,
           next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.abs(startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Inversion (startPoint: Point,
                 size: HW,
                 next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.inversion(startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}

class Grayscale (startPoint: Point,
                 size: HW,
                 next: Option[Operation] = None) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.grayscale(startPoint, size, performPixelValuesCheck(next))
    callNextIfExistsOrReturnResult(res, next)
  }
}
