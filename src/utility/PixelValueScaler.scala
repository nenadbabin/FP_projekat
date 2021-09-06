package utility

import picture.Pixel

object PixelValueScaler {
  def scaleFrom8BitToZeroToOne(red: Double, green: Double, blue: Double): Pixel = {
    val oldMax: Double = 255.0
    val oldMin: Double = 0.0
    val newMax: Double = 1.0
    val newMin: Double = 0.0
    val oldRange: Double = oldMax - oldMin
    val newRange: Double = newMax - newMin
    val newRed: Double = (((red - oldMin) * newRange) / oldRange) + newMin
    val newGreen: Double = (((green - oldMin) * newRange) / oldRange) + newMin
    val newBlue: Double = (((blue - oldMin) * newRange) / oldRange) + newMin

    new Pixel(newRed, newGreen, newBlue)
  }
}
