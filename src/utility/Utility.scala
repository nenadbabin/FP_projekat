package utility

import picture.{Picture, Pixel, PixelValueScaler}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Utility {
  def readPictureFromPath(path: String): Picture = {
    val file: File = new File(path)
    readPictureFromFile(file)
  }

  def readPictureFromFile(file: File): Picture = {
    val img: BufferedImage = ImageIO.read(file)
    readPictureFromBufferedImage(img)
  }

  def readPictureFromBufferedImage(img: BufferedImage): Picture = {
    val w = img.getWidth
    val h = img.getHeight

    val picture: Picture = new Picture(new HW(h, w))

    for (x <- 0 until w;
         y <- 0 until h) {
      val color = img.getRGB(w - x - 1, y)
      val red = (color & 0xff0000) / 65536
      val green = (color & 0xff00) / 256
      val blue = (color & 0xff)
      picture.pixels(y)(x) = PixelValueScaler.scaleFrom8BitToZeroToOne(red, green, blue)
    }

//    val kernel = Array[Array[Double]](
//      Array[Double](-1, -1, -1),
//      Array[Double](-1, 8, -1),
//      Array[Double](-1, -1, -1),
//    )
//    picture.convolution(kernel)

//    picture.median(new HW(3, 3))

    picture
  }
}
