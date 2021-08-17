package controller

import javafx.scene.Group
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

class SelectionDrawController {
  var starting_point_x = .0
  var starting_point_y = .0
  val group_for_rectangles = new Group
  var new_rectangle: Rectangle = null
  var new_rectangle_is_being_drawn = false
  val rectangle_colors: Array[Color] = Array(Color.TEAL, Color.TOMATO, Color.TURQUOISE, Color.VIOLET, Color.YELLOWGREEN, Color.GOLD)
  var color_index = 0

  def adjust_rectangle_properties(starting_point_x: Double, starting_point_y: Double, ending_point_x: Double, ending_point_y: Double, given_rectangle: Rectangle): Unit = {
    given_rectangle.setX(starting_point_x)
    given_rectangle.setY(starting_point_y)
    given_rectangle.setWidth(ending_point_x - starting_point_x)
    given_rectangle.setHeight(ending_point_y - starting_point_y)
    if (given_rectangle.getWidth < 0) {
      given_rectangle.setWidth(-given_rectangle.getWidth)
      given_rectangle.setX(given_rectangle.getX - given_rectangle.getWidth)
    }
    if (given_rectangle.getHeight < 0) {
      given_rectangle.setHeight(-given_rectangle.getHeight)
      given_rectangle.setY(given_rectangle.getY - given_rectangle.getHeight)
    }
  }


}
