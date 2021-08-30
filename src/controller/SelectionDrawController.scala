package controller

import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.shape.{Rectangle => JavaFXRectangle}
import utility.{HW, Point, Rectangle}

class SelectionDrawController {
  var starting_point_x: Double = 0.0
  var starting_point_y : Double = 0.0
  var new_rectangle: JavaFXRectangle = null
  var new_rectangle_is_being_drawn: Boolean = false
  var isUserDrawingSelections: Boolean = false

  def onMousePressed(event: MouseEvent): Unit = {
    if (isUserDrawingSelections) {
      if (!new_rectangle_is_being_drawn) {
        starting_point_x = event.getX
        starting_point_y = event.getY
        new_rectangle = new JavaFXRectangle
        new_rectangle.setStroke(Color.BLACK)
        new_rectangle_is_being_drawn = true
      }
    }
  }

  def adjustRectProperties(event: MouseEvent): Unit = {
    if (new_rectangle_is_being_drawn) {
      val current_ending_point_x = event.getX
      val current_ending_point_y = event.getY
      adjust_rectangle_properties(starting_point_x, starting_point_y, current_ending_point_x, current_ending_point_y, new_rectangle)
    }
  }

  def setOnMouseReleased(event: MouseEvent): Option[Rectangle] = {
    if (isUserDrawingSelections) {
      if (new_rectangle_is_being_drawn) {
        val x: Int = new_rectangle.getX.toInt
        val y: Int = new_rectangle.getY.toInt
        val height: Int = new_rectangle.getHeight.toInt
        val width: Int = new_rectangle.getWidth.toInt
        val rectToAdd: Rectangle = new Rectangle(new Point(x, y), new HW(height, width))

        new_rectangle = null
        new_rectangle_is_being_drawn = false

        return Some(rectToAdd)
      }
    }
    None
  }

  private def adjust_rectangle_properties(startingPointX: Double, startingPointY: Double, endingPointX: Double, endingPointY: Double, rect: JavaFXRectangle): Unit = {
    rect.setX(startingPointX)
    rect.setY(startingPointY)
    rect.setWidth(endingPointX - startingPointX)
    rect.setHeight(endingPointY - startingPointY)
    if (rect.getWidth < 0) {
      rect.setWidth(-rect.getWidth)
      rect.setX(rect.getX - rect.getWidth)
    }
    if (rect.getHeight < 0) {
      rect.setHeight(-rect.getHeight)
      rect.setY(rect.getY - rect.getHeight)
    }
  }

}
