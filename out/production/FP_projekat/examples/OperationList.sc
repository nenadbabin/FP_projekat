import picture.{Add, Mul, Operation, Picture, Sub}
import utility.{HW, Point}

val pic: Picture = new Picture(new HW(4, 5))
val list: List[Operation] = List(new Add(0.5, new Point(1, 1), pic.dim,
                                 new Add(0.1, new Point(1, 1), pic.dim)),
                                 new Sub(0.3, new Point(2, 2), pic.dim))
val newPic: Picture = (pic /: list)((picture: Picture, elem: Operation) => {elem.apply(picture)})
println(newPic)

class ComposedOperation (val const: Double,
                         startPoint: Point,
                         size: HW,
                         next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = picture.add(const, startPoint, size).mul(const, startPoint, size, performPixelValuesCheck(next))
    if (next != null) next.apply(res)
    else res
  }
}

class ComposedOperation2 (val const: Double,
                         startPoint: Point,
                         size: HW,
                         next: Operation = null) extends Operation (startPoint, size, next) {
  override def apply(picture: Picture): Picture = {
    val res: Picture = new Add(const, startPoint, size, new Mul(const, startPoint, size)) apply picture
    res
  }
}

val pic1: Picture = new Picture(new HW(4, 5))
val newOp: Operation = new ComposedOperation(0.5, new Point(1, 1), pic.dim,
                       new Add(0.2, new Point(0, 0), pic.dim))
val newPic1: Picture = newOp apply pic1
newPic1.median(new HW(1, 1), new Point(0, 0), newPic1.dim)

val pic2: Picture = new Picture(new HW(4, 5))
val newOp2: Operation = new ComposedOperation2(0.5, new Point(1, 1), pic.dim)
val newPic2: Picture = newOp2 apply pic