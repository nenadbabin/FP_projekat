package main

import javafx.application.Application
import javafx.collections.{FXCollections, ObservableList}
import javafx.geometry.{Insets, Pos}
import javafx.scene.canvas.Canvas
import javafx.scene.control.{Button, ComboBox, Label, RadioButton, TextField, ToggleGroup}
import javafx.scene.image.PixelWriter
import javafx.scene.layout.{BorderPane, HBox, VBox}
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
    bPane.setTop(this.createTopPane());
    bPane.setBottom(new TextField("Logger..."));
    bPane.setLeft(this.createLeftPane());
    bPane.setRight(this.createRightPane());

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

    val activeLayer = new HBox()
    val activeLayerName = new Label("...")
    activeLayer.getChildren.addAll(new Label("Layer name: "), activeLayerName)
    activeLayer.setAlignment(Pos.CENTER)

    val transparency = new HBox()
    val transparencyValueField = new TextField("0.1")
    transparencyValueField.setMaxWidth(35)
    transparency.getChildren.addAll(new Label("Transparency: "), transparencyValueField)
    transparency.setAlignment(Pos.CENTER)

    val moveUpButton = new Button()
    moveUpButton.setText("Move up")
    val moveDownButton = new Button()
    moveDownButton.setText("Move down")

    leftPane.setAlignment(Pos.CENTER)
    VBox.setMargin(addLayerButton, new Insets(10, 20, 10, 20))

    VBox.setMargin(layersComboBox, new Insets(10, 20, 5, 20))
    VBox.setMargin(activeLayer, new Insets(5, 20, 5, 20))
    VBox.setMargin(transparency, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveUpButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveDownButton, new Insets(5, 20, 10, 20))

    leftPane.getChildren.addAll(addLayerButton, layersComboBox, activeLayer, transparency, moveUpButton, moveDownButton)

    leftPane
  }

  def createRightPane(): VBox = {
    val addSelectionButton = new Button("Add selection...")
    val options: ObservableList[String] = FXCollections.observableArrayList("Option 1", "Option 2", "Option 3")
    val selectionsComboBox = new ComboBox[String](options)

    val rightPane = new VBox()

    val activeSelection = new HBox()
    val activeLayerName = new TextField()
    activeLayerName.setMaxWidth(100)
    activeSelection.getChildren.addAll(new Label("Selection name: "), activeLayerName)

    val activeSelect = new HBox()
    val group = new ToggleGroup()
    val rb1 = new RadioButton("Yes")
    val rb2 = new RadioButton("No")
    rb1.setToggleGroup(group)
    rb2.setToggleGroup(group)
    activeSelect.getChildren.addAll(new Label("Active: "), rb1, rb2)
    activeSelect.setAlignment(Pos.CENTER)

    val applyChangesButton = new Button()
    applyChangesButton.setText("Apply changes")

    val deleteSelectionButton = new Button()
    deleteSelectionButton.setText("Delete selection")

    VBox.setMargin(addSelectionButton, new Insets(10, 20, 10, 20))
    VBox.setMargin(selectionsComboBox, new Insets(10, 20, 5, 20))
    VBox.setMargin(activeSelection, new Insets(5, 20, 5, 20))
    VBox.setMargin(activeSelect, new Insets(5, 20, 5, 20))
    VBox.setMargin(applyChangesButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(deleteSelectionButton, new Insets(5, 20, 5, 20))

    rightPane.setAlignment(Pos.CENTER)

    rightPane.getChildren.addAll(addSelectionButton, selectionsComboBox, activeSelection, activeSelect, applyChangesButton, deleteSelectionButton)

    rightPane
  }

  def createTopPane(): HBox = {
    val topPane = new HBox()

    val loadProjectButton = new Button()
    loadProjectButton.setText("Load...")
    val saveProjectButton = new Button()
    saveProjectButton.setText("Save As...")

    HBox.setMargin(loadProjectButton, new Insets(20, 20, 20, 20))
    HBox.setMargin(saveProjectButton, new Insets(20, 20, 20, 0))

    topPane.setAlignment(Pos.CENTER_LEFT)

    topPane.getChildren.addAll(loadProjectButton, saveProjectButton)

    topPane
  }
}
