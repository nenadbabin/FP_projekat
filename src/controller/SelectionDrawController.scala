package controller

import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.shape.{Rectangle => JavaFXRectangle}
import utility.{HW, Point, Rectangle}

class SelectionDrawController {
  var startingPointX: Double = 0.0
  var startingPointY : Double = 0.0
  var newRectangle: Option[JavaFXRectangle] = None
  def rectangle: JavaFXRectangle = newRectangle match {case Some(nr) => nr}
  var isNewRectangleBeingDrawn: Boolean = false
  var isUserDrawingSelections: Boolean = false

  def onMousePressed(event: MouseEvent): Unit = {
    if (isUserDrawingSelections && !isNewRectangleBeingDrawn) {
      startingPointX = event.getX
      startingPointY = event.getY
      newRectangle = Some(new JavaFXRectangle)
      isNewRectangleBeingDrawn = true
    }
  }

  def adjustRectProperties(event: MouseEvent, canvasHeight: Double, canvasWidth: Double): Unit = {
    if (isUserDrawingSelections && isNewRectangleBeingDrawn) {
      def max (a: Double, b: Double): Double = if (b > a) b else a
      def min (a: Double, b: Double): Double = if (a > b) b else a
      val currentEndingPointX = min(max(event.getX, 0), canvasWidth)
      val currentEndingPointY = min(max(event.getY, 0), canvasHeight)
      adjustRectangleProperties(startingPointX, startingPointY, currentEndingPointX, currentEndingPointY)
    }
  }

  def setOnMouseReleased(event: MouseEvent): Option[Rectangle] = {
    if (isUserDrawingSelections && isNewRectangleBeingDrawn) {
      val x: Int = rectangle.getX.toInt
      val y: Int = rectangle.getY.toInt
      val height: Int = rectangle.getHeight.toInt
      val width: Int = rectangle.getWidth.toInt
      val rectToAdd: Rectangle = new Rectangle(new Point(x, y), new HW(height, width))

      newRectangle = None
      isNewRectangleBeingDrawn = false

      return Some(rectToAdd)
    }
    None
  }

  private def adjustRectangleProperties(startingPointX: Double, startingPointY: Double, endingPointX: Double, endingPointY: Double): Unit = {
    rectangle.setX(startingPointX)
    rectangle.setY(startingPointY)
    rectangle.setWidth(endingPointX - startingPointX)
    rectangle.setHeight(endingPointY - startingPointY)
    if (rectangle.getWidth < 0) {
      rectangle.setWidth(-rectangle.getWidth)
      rectangle.setX(rectangle.getX - rectangle.getWidth)
    }
    if (rectangle.getHeight < 0) {
      rectangle.setHeight(-rectangle.getHeight)
      rectangle.setY(rectangle.getY - rectangle.getHeight)
    }
  }
}
