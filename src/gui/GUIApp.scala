package gui

import main.JavaFXTest

import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JFrame
import javax.swing.JScrollPane
import javax.swing.JTextArea

object GUIApp extends App {

  val textArea = new JTextArea
  textArea.setText("Hello, Swing world")
  val scrollPane = new JScrollPane(textArea)

  val frame = new JFrame("Hello, Swing")
  frame.getContentPane.add(scrollPane, BorderLayout.CENTER)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

}

import javafx.application.Application
import javafx.scene.input.MouseEvent
import javafx.scene._
import javafx.stage.Stage
import javafx.geometry._
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle


object DrawingRectanglesFX { //  The following method adjusts coordinates so that the rectangle
  //  is shown "in a correct way" in relation to the mouse movement.
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[DrawingRectanglesFX], args: _*)
  }
}

class DrawingRectanglesFX extends Application {
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

  override def start(stage: Stage): Unit = {
    stage.setTitle("DrawingRectanglesFX.java")
    val scene = new Scene(group_for_rectangles, 800, 600)
    scene.setFill(Color.BEIGE)
    scene.setOnMousePressed((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (new_rectangle_is_being_drawn == false) {
          starting_point_x = event.getSceneX
          starting_point_y = event.getSceneY
          new_rectangle = new Rectangle
          // A non-finished rectangle has always the same color.
          new_rectangle.setFill(Color.SNOW) // almost white color

          new_rectangle.setStroke(Color.BLACK)
          group_for_rectangles.getChildren.add(new_rectangle)
          new_rectangle_is_being_drawn = true
        }
      }

      foo(event)
    })
    scene.setOnMouseDragged((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (new_rectangle_is_being_drawn == true) {
          val current_ending_point_x = event.getSceneX
          val current_ending_point_y = event.getSceneY
          adjust_rectangle_properties(starting_point_x, starting_point_y, current_ending_point_x, current_ending_point_y, new_rectangle)
        }
      }

      foo(event)
    })
    scene.setOnMouseReleased((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (new_rectangle_is_being_drawn == true) { // Now the drawing of the new rectangle is finished.
          // Let's set the final color for the rectangle.
          new_rectangle.setFill(rectangle_colors(color_index))
          color_index += 1 // Index for the next color to use.

          // If all colors have been used we'll start re-using colors from the
          // beginning of the array.
          if (color_index >= rectangle_colors.length) color_index = 0
          new_rectangle = null
          new_rectangle_is_being_drawn = false
        }
      }

      foo(event)
    })
    stage.setScene(scene)
    stage.show()
  }
}
