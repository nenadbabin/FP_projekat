import picture.{Picture, Pixel}
import utility.{HW, Point}

//var newPixel: Pixel = new Pixel(0.0, 0.0, 0.0)
//newPixel = newPixel.add(0.5) // 0.0 + 0.5 = 0.5
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//newPixel = newPixel.sub(0.2) // 0.5 - 0.2 = 0.3
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//newPixel = newPixel.inverseSub(0.5) // 0.5 - 0.3 = 0.2
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//newPixel = newPixel.mul(2) // 0.2 * 2 = 0.4
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//newPixel = newPixel.div(2) // 0.4 / 2 = 0.2
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//newPixel = newPixel.reverseDiv(0.1) // 0.1 / 0.2 = 0.5
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)
//
//newPixel = new Pixel(0.0, 0.0, 0.0)
//newPixel = (newPixel add 1 sub 0.5 mul 0.5 div 2 min 0.001 max 0.7).abs().log()
//println(newPixel.r + " " + newPixel.g + " " + newPixel.b)

var pic: Picture = new Picture(new HW(4, 6))
println(pic)
pic.min(0)
println(pic)
pic.add(0.5)
pic.sub(0.2, new Point(1, 1), new HW(2, 2))
println(pic)
