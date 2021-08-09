package main

import javafx.application.Application
import javafx.collections.{FXCollections, ObservableList}
import javafx.geometry.{Insets, Pos}
import javafx.scene.canvas.Canvas
import javafx.scene.control.{Button, ComboBox, TextField}
import javafx.scene.image.PixelWriter
import javafx.scene.layout.{BorderPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.{Group, Scene}
import javafx.stage.Stage
import picture.{Picture, Pixel}
import selection.Selection
import utility.{HW, Point, Rectangle, Utility}


object JavaFXTest {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[JavaFXTest], args: _*)
  }
}

class JavaFXTest extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("PhotoEditor")
    val root = new Group()
    val canvas = new Canvas()

    val bPane = new BorderPane()
    bPane.setCenter(canvas)
    bPane.setTop(new TextField("Top"));
    bPane.setBottom(new TextField("Bottom"));
    bPane.setLeft(this.createLeftPane());
    bPane.setRight(new TextField("Right"));

    val picture: Picture = Utility.readPictureFromFile("pictures/lena.jpg")

    canvas.setWidth(picture.dim.width)
    canvas.setHeight(picture.dim.height)

    println(picture.dim)

    val pixelWriter: PixelWriter = canvas.getGraphicsContext2D.getPixelWriter

    val rect1: Rectangle = new Rectangle(new Point(0,0), new HW(256,256))
    val rect2: Rectangle = new Rectangle(new Point(256,256), new HW(256,256))
    val rect_list: List[Rectangle] = List[Rectangle](rect1, rect2)
    val selection: Selection = new Selection("sel_1", rect_list)

    for (rect <- selection.rectangles) {
      picture.toGrayscale(rect.topLeftCorner, rect.dim)
    }

    for (y <- 0 until picture.dim.height;
         x <- 0 until picture.dim.width) {
      val pixel: Pixel = picture.pixels(y)(x)
      val r = pixel.r
      val g = pixel.g
      val b = pixel.b
      pixelWriter.setColor(x, y, new Color(r, g, b, 1))
    }

    root.getChildren.add(bPane)
    primaryStage.setScene(new Scene(root))
    primaryStage.show()
  }

  def createLeftPane(): VBox = {
    val addLayerButton = new Button("Add layer...")
    val options: ObservableList[String] = FXCollections.observableArrayList("Option 1", "Option 2", "Option 3")
    val layersComboBox = new ComboBox[String](options)

    val leftPane = new VBox()

    leftPane.setAlignment(Pos.CENTER)
    VBox.setMargin(addLayerButton, new Insets(20, 20, 5, 20))
    leftPane.getChildren.addAll(addLayerButton, layersComboBox)

    leftPane
  }
}
