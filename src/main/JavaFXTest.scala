package main

import controller.HomeController
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.{Insets, Pos}
import javafx.scene.canvas.Canvas
import javafx.scene.control._
import javafx.scene.layout.{BorderPane, HBox, Pane, VBox}
import javafx.scene.{Group, Node, Scene}
import javafx.stage.{FileChooser, Stage}
import layer.Layer
import picture.Picture
import utility.Utility

import java.io.File


object JavaFXTest {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[JavaFXTest], args: _*)
  }
}

class JavaFXTest extends Application {

  val homeController: HomeController = new HomeController()
  val bPane = new BorderPane()

  private def setNewCanvas(pane: Pane): Unit = {
    bPane.setCenter(null)
    bPane.setCenter(pane)
  }

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("PhotoEditor")
    val root = new Group()
    val canvas = new Canvas()

    bPane.setCenter(canvas)
    bPane.setTop(this.createTopPane())
    bPane.setBottom(new TextField("Logger..."))
    bPane.setLeft(this.createLeftPane(canvas))
    bPane.setRight(this.createRightPane())

    canvas.setWidth(512)
    canvas.setHeight(512)

    root.getChildren.add(bPane)
    primaryStage.setScene(new Scene(root))
    primaryStage.show()
  }

  def createLeftPane(canvas: Canvas): VBox = {
    val addLayerButton = new Button("Add layer...")

    val options: ObservableList[String] = FXCollections.observableArrayList()
    val layersComboBox = new ComboBox[String](options)

    // Add listener for creating new layer (opening picture)
    val event: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val fileChooser: FileChooser = new FileChooser()
        fileChooser.setTitle("Open picture...")
        val stage: Stage = (e.getSource.asInstanceOf[Node]).getScene.getWindow.asInstanceOf[Stage]
        val file: File = fileChooser.showOpenDialog(stage)
        if (file != null) {
          val picture: Picture = Utility.readPictureFromFile(file)
          val new_layer: Layer = new Layer(picture, file.getName)
          homeController.add_layer(new_layer)
          setNewCanvas(homeController.draw())
          options.add(new_layer.name)
        }
      }
    }
    addLayerButton.setOnAction(event)

    val leftPane = new VBox()

    val activeLayer = new HBox()
    val activeLayerName = new Label("...")
    activeLayer.getChildren.addAll(new Label("Layer name: "), activeLayerName)
    activeLayer.setAlignment(Pos.CENTER)

    val transparency = new HBox()
    val transparencyValueField = new TextField("")
    transparencyValueField.setMaxWidth(35)
    transparency.getChildren.addAll(new Label("Transparency: "), transparencyValueField)
    transparency.setAlignment(Pos.CENTER)


    val applyTransparencyButton = new Button()
    applyTransparencyButton.setText("Apply transparency")
    val moveUpButton = new Button()
    moveUpButton.setText("Move up")
    val moveDownButton = new Button()
    moveDownButton.setText("Move down")

    // Add listener for layer selection change
    layersComboBox.valueProperty.addListener(new ChangeListener[String]() {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val layer: Layer = homeController.findLayerByName(newValue)
        transparencyValueField.setText(layer.transparency.toString)
        activeLayerName.setText(layer.name)
      }
    })

    // Change transparency
    val applyTransparencyEvent: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        try {
          val layerTransparency: Double = transparencyValueField.getText.toDouble
          val layerName: String = activeLayerName.getText
          val layer: Layer = homeController.findLayerByName(layerName)
          if (layer != null) {
            if (layerTransparency >= 0.0 && layerTransparency <= 1.0) {
              layer.transparency = layerTransparency
              setNewCanvas(homeController.draw())
            } else {
              // TODO: Print error to logger
            }
          }
        } catch {
          case e : NumberFormatException => return // TODO: Print error to logger
        }
      }
    }
    applyTransparencyButton.setOnAction(applyTransparencyEvent)

    // Move layer backwards
    val moveLayerBackwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layer: Layer = homeController.findLayerByName(layerName)
        if (layer != null) {
          homeController.moveLayerBackwards(layer)
          setNewCanvas(homeController.draw())
        }
      }
    }
    moveDownButton.setOnAction(moveLayerBackwards)

    // Move layer forwards
    val moveLayerForwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layer: Layer = homeController.findLayerByName(layerName)
        if (layer != null) {
          homeController.moveLayerForwards(layer)
          setNewCanvas(homeController.draw())
        }
      }
    }
    moveUpButton.setOnAction(moveLayerForwards)

    leftPane.setAlignment(Pos.CENTER)
    VBox.setMargin(addLayerButton, new Insets(10, 20, 10, 20))

    VBox.setMargin(layersComboBox, new Insets(10, 20, 5, 20))
    VBox.setMargin(activeLayer, new Insets(5, 20, 5, 20))
    VBox.setMargin(transparency, new Insets(5, 20, 5, 20))
    VBox.setMargin(applyTransparencyButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveUpButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveDownButton, new Insets(5, 20, 10, 20))

    leftPane.getChildren.addAll(addLayerButton, layersComboBox, activeLayer, transparency, applyTransparencyButton, moveUpButton, moveDownButton)

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
