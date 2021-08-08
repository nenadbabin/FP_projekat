package main

import picture.{Picture, Pixel}
import selection.Selection
import utility.{HW, Point, Rectangle, Utility}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Main extends App {

  val pic: Picture = new Picture(new HW(4, 5))
  val rect1: Rectangle = new Rectangle(new Point(1,0), new HW(10,10))
  val rect_list: List[Rectangle] = List(rect1)
  val selection: Selection = new Selection("sel_1", rect_list)

  for (rect <- selection.rectangles) {
    pic.toGrayscale(rect.topLeftCorner, rect.dim)
  }

  println(pic)
}

object MainTestReadJPG extends App {
  val picture: Picture = Utility.readPictureFromFile("pictures/lena.jpg")
  println(picture)
}
