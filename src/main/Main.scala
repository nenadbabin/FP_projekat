package main

import picture.{Operation, Picture, Pixel, Add, Sub}
import selection.Selection
import utility.{HW, Point, Rectangle, Utility}

object Main extends App {

  val pic: Picture = new Picture(new HW(4, 5))
  val rect1: Rectangle = new Rectangle(new Point(1,0), new HW(10,10))
  val rect_list: List[Rectangle] = List[Rectangle](rect1)
  val selection: Selection = new Selection("sel_1", rect_list)

  for (rect <- selection.rectangles) {
    pic.grayscale(rect.topLeftCorner, rect.dim)
  }

  println(pic)
}

object MainTestReadJPG extends App {
  val picture: Picture = Utility.readPictureFromPath("pictures/lena.jpg")
  println(picture)
}


object PictureRefactor extends App {
  var pic: Picture = new Picture(new HW(4, 6))
  println(pic)
  pic.min(0)
  println(pic)
}

object Conv extends App {
  var pic: Picture = new Picture(new HW(4, 6))
  println(pic)
  val kernel = Array[Array[Double]](
    Array[Double](-1, -1, -1),
    Array[Double](-1, 8, -1),
    Array[Double](-1, -1, -1),
  )
  pic.convolution(kernel)
  println(pic)
}

object OpList extends App {
  val pic: Picture = new Picture(new HW(4, 5))
  val list: List[Operation] = List(new Add(0.5, new Point(1, 1), pic.dim, new Add(0.1, new Point(1, 1), pic.dim)),
                                   new Sub(0.3, new Point(2, 2), pic.dim))
  val newPic: Picture = (pic /: list)((picture: Picture, elem: Operation) => {elem.apply(picture)})
  println(newPic)

  class ComposedOperation (val const: Double,
                           startPoint: Point,
                           size: HW,
                           next: Operation = null) extends Operation (startPoint, size, next) {
    override def apply(picture: Picture): Picture = {
      val res: Picture = picture.add(const, startPoint, size).mul(const, startPoint, size)
      if (next != null) next.apply(res)
      else res
    }
  }

  val pic1: Picture = new Picture(new HW(4, 5))
  val newOp: Operation = new ComposedOperation(0.5, new Point(1, 1), pic.dim,
                         new Add(0.2, new Point(0, 0), pic.dim))
  val newPic1: Picture = newOp.apply(pic1)
  println(pic1)
}