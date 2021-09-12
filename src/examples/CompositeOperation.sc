import picture.{Add, Div, Mul, Operation, Picture, Sub}
import utility.{HW, Point}

val pic: Picture = new Picture(new HW(4, 5))
val compositeOperation: Operation = new Add(4, new Point(0, 0), pic.dim,
                        new Sub(3.5, new Point(0, 0), pic.dim,
                        new Mul(3, new Point(0, 0), pic.dim,
                        new Div(10, new Point(0, 0), pic.dim))))
compositeOperation.apply(pic)
println(pic)
