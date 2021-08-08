package utility

import picture.{Picture, Pixel}

import java.io.File
import javax.imageio.ImageIO

object Utility {
  def readPictureFromFile(path: String): Picture = {
    val img = ImageIO.read(new File(path))

    val w = img.getWidth
    val h = img.getHeight

    val picture: Picture = new Picture(new HW(h, w))

    for (x <- 0 until w;
         y <- 0 until h) {
      val color = img.getRGB(w - x - 1, y)
      val red = (color & 0xff0000) / 65536
      val green = (color & 0xff00) / 256
      val blue = (color & 0xff)
      picture.pixels(y)(x) = new Pixel(red, green, blue)
    }

    picture
  }
}
