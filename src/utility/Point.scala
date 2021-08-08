package utility

class Point (val x: Int,
             val y: Int) {
  require(x >= 0)
  require(y >= 0)
}
