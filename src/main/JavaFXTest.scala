package main

import controller.{LayersController, SelectionDrawController, SelectionsController}
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.{Insets, Pos}
import javafx.scene.canvas.Canvas
import javafx.scene.control.{TextInputDialog, _}
import javafx.scene.input.MouseEvent
import javafx.scene.layout._
import javafx.scene.paint.Color
import javafx.scene.shape.{Rectangle => JavaFXRectangle}
import javafx.scene.{Group, Node, Scene}
import javafx.stage.{FileChooser, Stage}
import layer.Layer
import picture.Picture
import selection.{BaseSelection, FlexibleSelection, Selection}
import utility.{HW, Point, Rectangle, Utility}

import java.io.File
import scala.util.matching.Regex


object JavaFXTest {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[JavaFXTest], args: _*)
  }
}

class JavaFXTest extends Application {

  // Controllers
  val layersController: LayersController = new LayersController()
  val selectionDrawController: SelectionDrawController = new SelectionDrawController()
  val selectionsController: SelectionsController = new SelectionsController()

  // Canvas with pictures
  val bPane = new BorderPane()
  val canvasOverlay = new Canvas()

  // Drawing rectangles for selections
  var isUserDrawingSelections: Boolean = false
  var drawnRectangles: List[Rectangle] = List[Rectangle]()

  private def setNewCanvas(pane: Pane): Unit = {
    if (pane.getChildren.size() == 0) {
      canvasOverlay.setHeight(0)
      canvasOverlay.setWidth(0)
    } else {
      val paneHeight = pane.getChildren.get(0).asInstanceOf[Canvas].getHeight
      val paneWidth = pane.getChildren.get(0).asInstanceOf[Canvas].getWidth
      canvasOverlay.setHeight(paneHeight)
      canvasOverlay.setWidth(paneWidth)
    }

    val holder: StackPane = new StackPane()
    for (i <- 0 until pane.getChildren.size()) {
      holder.getChildren.add(pane.getChildren.get(0))
    }
    holder.getChildren.add(canvasOverlay)

    bPane.setCenter(null)
    bPane.setCenter(holder)
  }

  // Clear canvas for drawing rectangles
  private def clearDrawingCanvas(): Unit = {
    val gc = canvasOverlay.getGraphicsContext2D
    gc.clearRect(0, 0, canvasOverlay.getWidth, canvasOverlay.getHeight)
  }

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("PhotoEditor")
    val root = new Group()

    bPane.setCenter(canvasOverlay)
    bPane.setTop(this.createTopPane())
    bPane.setBottom(new TextField("Logger..."))
    bPane.setLeft(this.createLeftPane())
    bPane.setRight(this.createRightPane())

    canvasOverlay.setWidth(512)
    canvasOverlay.setHeight(512)

    canvasOverlay.setOnMousePressed((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (isUserDrawingSelections) {
          if (selectionDrawController.new_rectangle_is_being_drawn == false) {
            selectionDrawController.starting_point_x = event.getX
            selectionDrawController.starting_point_y = event.getY
            selectionDrawController.new_rectangle = new JavaFXRectangle
            // A non-finished rectangle has always the same color.
            selectionDrawController.new_rectangle.setFill(Color.SNOW) // almost white color

            selectionDrawController.new_rectangle.setStroke(Color.BLACK)
            selectionDrawController.group_for_rectangles.getChildren.add(selectionDrawController.new_rectangle)
            selectionDrawController.new_rectangle_is_being_drawn = true
          }
        }
      }

      foo(event)
    })
    canvasOverlay.setOnMouseDragged((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (isUserDrawingSelections) {
          if (selectionDrawController.new_rectangle_is_being_drawn == true) {
            val current_ending_point_x = event.getX
            val current_ending_point_y = event.getY
            selectionDrawController.adjust_rectangle_properties(selectionDrawController.starting_point_x, selectionDrawController.starting_point_y, current_ending_point_x, current_ending_point_y, selectionDrawController.new_rectangle)
            clearDrawingCanvas()
            val gc = canvasOverlay.getGraphicsContext2D

            for (r <- drawnRectangles)  {
              val gc = canvasOverlay.getGraphicsContext2D
              gc.strokeRoundRect(r.topLeftCorner.x, r.topLeftCorner.y, r.dim.width, r.dim.height, 1, 1)
            }

            gc.strokeRoundRect(selectionDrawController.new_rectangle.getX,
              selectionDrawController.new_rectangle.getY,
              selectionDrawController.new_rectangle.getWidth,
              selectionDrawController.new_rectangle.getHeight, 1, 1)
          }
        }
      }

      foo(event)
    })
    canvasOverlay.setOnMouseReleased((event: MouseEvent) => {
      def foo(event: MouseEvent) = {
        if (isUserDrawingSelections) {
          if (selectionDrawController.new_rectangle_is_being_drawn == true) { // Now the drawing of the new rectangle is finished.
            // Let's set the final color for the rectangle.
            selectionDrawController.new_rectangle.setFill(selectionDrawController.rectangle_colors(selectionDrawController.color_index))
            selectionDrawController.color_index += 1 // Index for the next color to use.

            // If all colors have been used we'll start re-using colors from the
            // beginning of the array.
            val x: Int = selectionDrawController.new_rectangle.getX.toInt
            val y: Int = selectionDrawController.new_rectangle.getY.toInt
            val height: Int = selectionDrawController.new_rectangle.getHeight.toInt
            val width: Int = selectionDrawController.new_rectangle.getWidth.toInt
            val rectToAdd: Rectangle = new Rectangle(new Point(x, y), new HW(height, width))
            drawnRectangles = rectToAdd :: drawnRectangles

            if (selectionDrawController.color_index >= selectionDrawController.rectangle_colors.length) selectionDrawController.color_index = 0
            selectionDrawController.new_rectangle = null
            selectionDrawController.new_rectangle_is_being_drawn = false
          }
        }
      }

      foo(event)
    })

    root.getChildren.add(bPane)
    primaryStage.setScene(new Scene(root))
    primaryStage.show()
  }

  def createLeftPane(): VBox = {
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
        val fileName: String = file.getName
        val newFileName: String =
          if (layersController.findLayerByName(fileName) != null) {
            s"${fileName}_${layersController.countLayersWithSimilarNames(fileName)}"
          } else {
            fileName
          }
        if (file != null) {
          val picture: Picture = Utility.readPictureFromFile(file)
          val new_layer: Layer = new Layer(picture, newFileName)
          layersController.add_layer(new_layer)
          setNewCanvas(layersController.drawLayers())
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
    transparency.setSpacing(5)
    val transparencyValueField = new TextField("")
    transparencyValueField.setMaxWidth(35)
    transparency.getChildren.addAll(new Label("Transparency: "), transparencyValueField)
    transparency.setAlignment(Pos.CENTER)

    val activeSelect = new HBox()
    activeSelect.setSpacing(5)
    val group = new ToggleGroup()
    val rb1 = new RadioButton("Yes")
    val rb2 = new RadioButton("No")
    rb1.setToggleGroup(group)
    rb2.setToggleGroup(group)
    activeSelect.getChildren.addAll(new Label("Active: "), rb1, rb2)
    activeSelect.setAlignment(Pos.CENTER)

    val applyLayerChangesButton = new Button()
    applyLayerChangesButton.setText("Apply changes")
    val moveUpButton = new Button()
    moveUpButton.setText("Move up")
    val moveDownButton = new Button()
    moveDownButton.setText("Move down")

    // Add listener for layer selection change
    layersComboBox.valueProperty.addListener(new ChangeListener[String]() {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val layer: Layer = layersController.findLayerByName(newValue)
        transparencyValueField.setText(layer.transparency.toString)
        activeLayerName.setText(layer.name)
        if (layer.active) rb1.setSelected(true)
        else rb2.setSelected(true)
      }
    })

    // Apply layer changes
    val applyLayerChangesEvent: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        try {
          val layerTransparency: Double = transparencyValueField.getText.toDouble
          val layerName: String = activeLayerName.getText
          val layer: Layer = layersController.findLayerByName(layerName)
          if (layer != null) {
            if (layerTransparency >= 0.0 && layerTransparency <= 1.0) {
              layer.transparency = layerTransparency
              if (group.getSelectedToggle != null) {
                val toggleString: String = group.getSelectedToggle.asInstanceOf[RadioButton].getText
                if (toggleString == "Yes") {
                  layer.active = true
                }
                if (toggleString == "No") {
                  layer.active = false
                }
              }
              setNewCanvas(layersController.drawLayers())
            } else {
              // TODO: Print error to logger
            }
          }
        } catch {
          case e : NumberFormatException => return // TODO: Print error to logger
        }
      }
    }
    applyLayerChangesButton.setOnAction(applyLayerChangesEvent)

    // Move layer backwards
    val moveLayerBackwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layer: Layer = layersController.findLayerByName(layerName)
        if (layer != null) {
          layersController.moveLayerBackwards(layer)
          setNewCanvas(layersController.drawLayers())
        }
      }
    }
    moveDownButton.setOnAction(moveLayerBackwards)

    // Move layer forwards
    val moveLayerForwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layer: Layer = layersController.findLayerByName(layerName)
        if (layer != null) {
          layersController.moveLayerForwards(layer)
          setNewCanvas(layersController.drawLayers())
        }
      }
    }
    moveUpButton.setOnAction(moveLayerForwards)

    leftPane.setAlignment(Pos.CENTER)
    addLayerButton.setAlignment(Pos.CENTER)
    activeLayer.setAlignment(Pos.CENTER)
    transparency.setAlignment(Pos.CENTER)
    activeSelect.setAlignment(Pos.CENTER)
    applyLayerChangesButton.setAlignment(Pos.CENTER)
    moveUpButton.setAlignment(Pos.CENTER)
    moveDownButton.setAlignment(Pos.CENTER)
    VBox.setMargin(addLayerButton, new Insets(10, 20, 10, 20))
    VBox.setMargin(layersComboBox, new Insets(10, 20, 5, 20))
    VBox.setMargin(activeLayer, new Insets(5, 20, 5, 20))
    VBox.setMargin(transparency, new Insets(5, 20, 5, 20))
    VBox.setMargin(activeSelect, new Insets(5, 20, 5, 20))
    VBox.setMargin(applyLayerChangesButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveUpButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(moveDownButton, new Insets(5, 20, 10, 20))

    leftPane.getChildren.addAll(addLayerButton, layersComboBox, activeLayer, transparency, activeSelect, applyLayerChangesButton, moveUpButton, moveDownButton)

    leftPane
  }

  def createRightPane(): VBox = {

    val addSelectionButton: Button = new Button("Add selection...")

    val selectionsOptions: ObservableList[String] = FXCollections.observableArrayList("No selection")
    val selection: FlexibleSelection = new FlexibleSelection("No selection")
    selectionsController.addSelection(selection)
    val selectionsComboBox = new ComboBox[String](selectionsOptions)

    val selectionNameDialog: TextInputDialog = new TextInputDialog
    selectionNameDialog.setHeaderText("Choose name for the new selection.")
    selectionNameDialog.setTitle("Selection name")
    selectionNameDialog.setGraphic(null)
    val selectionDrawingEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        if (!isUserDrawingSelections) {
          selectionNameDialog.showAndWait
          val selectionName: String = selectionNameDialog.getEditor.getText
          if (selectionName == "") {
            // TODO: Print error to logger
            return
          }
          if (selectionsController.findSelectionByName(selectionName) != null) {
            // TODO: Print error to logger
            return
          }
          addSelectionButton.setText("Finish drawing")
          isUserDrawingSelections = true
          clearDrawingCanvas()
        } else {
          val selectionName: String = selectionNameDialog.getEditor.getText
          selectionsOptions.add(selectionName)
          val selection: Selection = new Selection(selectionName, drawnRectangles)
          selectionsController.addSelection(selection)
          addSelectionButton.setText("Add selection...")
          isUserDrawingSelections = false
          drawnRectangles = List[Rectangle]()
          clearDrawingCanvas()
        }

      }
    }
    addSelectionButton.setOnAction(selectionDrawingEvent)

    val rightPane = new VBox()

    val activeSelection = new HBox()
    val activeSelectionName = new Label("...")
    activeSelection.getChildren.addAll(new Label("Selection name: "), activeSelectionName)

    selectionsComboBox.valueProperty.addListener(new ChangeListener[String]() {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val selection: BaseSelection = selectionsController.findSelectionByName(newValue)
        activeSelectionName.setText(selection.name)
        clearDrawingCanvas()
        selection match {
          // Draw rectangles on canvas
          case s: Selection =>
            val gc = canvasOverlay.getGraphicsContext2D
            for (r <- s.rectangles) {
              gc.strokeRoundRect(r.topLeftCorner.x, r.topLeftCorner.y, r.dim.width, r.dim.height, 1, 1)
            }
          case _ =>
        }
      }
    })

    val colorPicker: HBox = new HBox()
    colorPicker.setSpacing(5)
    val redTxtFld: TextField = new TextField("0.0")
    redTxtFld.setMaxWidth(35)
    val greenTxtFld: TextField = new TextField("0.0")
    greenTxtFld.setMaxWidth(35)
    val blueTxtFld: TextField = new TextField("0.0")
    blueTxtFld.setMaxWidth(35)
    val colorPreview: JavaFXRectangle = new JavaFXRectangle(0, 0, 25, 25)
    colorPreview.setFill(new Color(0, 0, 0, 1))
    colorPreview.setStroke(Color.BLACK)

    colorPicker.getChildren.addAll(redTxtFld, greenTxtFld, blueTxtFld, colorPreview)

    val colorChangeListener: ChangeListener[String] = new ChangeListener[String] {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val redString: String = redTxtFld.getText
        val greenString: String = greenTxtFld.getText
        val blueString: String = blueTxtFld.getText

        try {
          val redVal: Double = redString.toDouble
          val greenVal: Double = greenString.toDouble
          val blueVal: Double = blueString.toDouble

          if (redVal >= 0 && redVal <= 1 &&
              greenVal >= 0 && greenVal <= 1 &&
              blueVal >= 0 && blueVal <= 1) {
            colorPreview.setFill(new Color(redVal, greenVal, blueVal, 1))
          }

        } catch {
          case e : NumberFormatException => return // TODO: Print error to logger
        }
      }
    }

    redTxtFld.textProperty().addListener(colorChangeListener)
    greenTxtFld.textProperty().addListener(colorChangeListener)
    blueTxtFld.textProperty().addListener(colorChangeListener)

    val applyChangesButton = new Button()
    applyChangesButton.setText("Apply color")

    val colorPickerEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selection: BaseSelection = selectionsController.findSelectionByName(activeSelectionName.getText)
        val color: Color = colorPreview.getFill.asInstanceOf[Color]
        selection.applyColor(layersController.layers, color.getRed, color.getGreen, color.getBlue)
        setNewCanvas(layersController.drawLayers())
      }
    }
    applyChangesButton.setOnAction(colorPickerEvent)

    val filterOptions: ObservableList[String] = FXCollections.observableArrayList(
      "Addition", "Subtraction", "Inverse subtraction",
      "Multiplication", "Division", "Inverse division",
      "Power", "Logarithm", "Absolute value",
      "Minimum", "Maximum", "Inversion",
      "Grayscale", "Median", "Sobel")
    val filtersComboBox = new ComboBox[String](filterOptions)

    val filterButton = new Button()
    filterButton.setText("Apply Filter")

    val filterConstTxtFld: TextField = new TextField("0.0")
    filterConstTxtFld.setMaxWidth(35)

    val filterWrapper: HBox = new HBox()
    filterWrapper.setSpacing(5)
    filterWrapper.getChildren.addAll(filtersComboBox, filterConstTxtFld)

    val filterEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selection: BaseSelection = selectionsController.findSelectionByName(activeSelectionName.getText)
        val filterString: String = filtersComboBox.getValue

        def getValueFromTextField: Option[Double] = {
          try {
            val text: String = filterConstTxtFld.getText
            Some(text.toDouble)
          } catch {
            case e : NumberFormatException => null // TODO: Print error to logger
          }
        }

        filterString match {
          case "Addition" if getValueFromTextField.isDefined => selection.add(getValueFromTextField.get, layersController.activeLayers)
          case "Subtraction" if getValueFromTextField.isDefined => selection.sub(getValueFromTextField.get, layersController.activeLayers)
          case "Inverse subtraction" if getValueFromTextField.isDefined => selection.inverseSub(getValueFromTextField.get, layersController.activeLayers)
          case "Multiplication" if getValueFromTextField.isDefined => selection.mul(getValueFromTextField.get, layersController.activeLayers)
          case "Division" if getValueFromTextField.isDefined => selection.div(getValueFromTextField.get, layersController.activeLayers)
          case "Inverse division" if getValueFromTextField.isDefined => selection.inverseDiv(getValueFromTextField.get, layersController.activeLayers)
          case "Power" if getValueFromTextField.isDefined => selection.pow(getValueFromTextField.get, layersController.activeLayers)
          case "Logarithm" => selection.log(layersController.layers)
          case "Absolute value" => selection.abs(layersController.layers)
          case "Minimum" if getValueFromTextField.isDefined => selection.min(getValueFromTextField.get, layersController.activeLayers)
          case "Maximum" if getValueFromTextField.isDefined => selection.max(getValueFromTextField.get, layersController.activeLayers)
          case "Inversion" => selection.inversion(layersController.activeLayers)
          case "Grayscale" => selection.grayscale(layersController.activeLayers)
          case "Median" => selection.median(layersController.activeLayers)
          case "Sobel" => selection.sobel(layersController.activeLayers)
        }
        setNewCanvas(layersController.drawLayers())
      }
    }
    filterButton.setOnAction(filterEvent)

    val deleteSelectionButton = new Button()
    deleteSelectionButton.setText("Delete selection")

    val deleteSelectionEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selectionName: String = activeSelectionName.getText
        val selection: BaseSelection = selectionsController.findSelectionByName(selectionName)
        selection match {
          case s: Selection =>
            // Delete selection
            selection.restore()
            selectionsController.removeSelection(selection)
            selectionsOptions.remove(selectionName)
            setNewCanvas(layersController.drawLayers())
          case _: FlexibleSelection =>
            // Restore selection, but don't delete it
            selection.restore()
            setNewCanvas(layersController.drawLayers())
          case _ =>
        }
      }
    }
    deleteSelectionButton.setOnAction(deleteSelectionEvent)

    addSelectionButton.setAlignment(Pos.CENTER)
    activeSelection.setAlignment(Pos.CENTER)
    colorPicker.setAlignment(Pos.CENTER)
    applyChangesButton.setAlignment(Pos.CENTER)
    filterButton.setAlignment(Pos.CENTER)
    filterWrapper.setAlignment(Pos.CENTER)
    deleteSelectionButton.setAlignment(Pos.CENTER)
    VBox.setMargin(addSelectionButton, new Insets(10, 20, 10, 20))
    VBox.setMargin(selectionsComboBox, new Insets(10, 20, 5, 20))
    VBox.setMargin(activeSelection, new Insets(5, 20, 5, 20))
    VBox.setMargin(colorPicker, new Insets(5, 20, 5, 20))
    VBox.setMargin(applyChangesButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(filterButton, new Insets(5, 20, 5, 20))
    VBox.setMargin(filterWrapper, new Insets(5, 20, 5, 20))
    VBox.setMargin(deleteSelectionButton, new Insets(5, 20, 5, 20))

    rightPane.setAlignment(Pos.CENTER)

    rightPane.getChildren.addAll(addSelectionButton, selectionsComboBox, activeSelection, colorPicker, applyChangesButton, filterWrapper, filterButton, deleteSelectionButton)

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
