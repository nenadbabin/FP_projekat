import picture.Pixel

val pixel1: Pixel = Pixel.sub(Pixel.add(new Pixel(0), 0.5), 0.3)
val pixel2: Pixel = Pixel.div(Pixel.mul(Pixel.inverseSub(new Pixel(0.3), 0.5), 2), 3)
def complexOp(pixel: Pixel): Pixel = Pixel.min(Pixel.pow(pixel, 3), 0.2)
val pixel3: Pixel = Pixel.abs(Pixel.log(Pixel.max(complexOp(new Pixel(0.9)), 0.3), checkRange = false))
val pixel24: Pixel = Pixel.grayscale(Pixel.applyColor(new Pixel(0), 0.1, 0.3, 0.9))