package main

import controller.{LayersController, SelectionDrawController, SelectionsController}
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList}
import javafx.embed.swing.SwingFXUtils
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.{Insets, Pos}
import javafx.scene.canvas.Canvas
import javafx.scene.control.{TextInputDialog, _}
import javafx.scene.image.WritableImage
import javafx.scene.input.MouseEvent
import javafx.scene.layout._
import javafx.scene.paint.Color
import javafx.scene.shape.{Rectangle => JavaFXRectangle}
import javafx.scene.{Group, Node, Scene}
import javafx.stage.{FileChooser, Stage}
import layer.Layer
import utility.LoggerMessageType.LoggerMessageType
import picture.Picture
import selection.{BaseSelection, FlexibleSelection, Selection}
import utility.{LoggerMessageType, Rectangle, Utility}

import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import java.time.LocalTime
import java.util.Base64
import javax.imageio.ImageIO
import scala.io.{BufferedSource, Source}
import scala.util.Using


object PhotoEditorApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[PhotoEditorApp], args: _*)
  }
}

class PhotoEditorApp extends Application {

  // Controllers
  val layersController: LayersController = new LayersController()
  val selectionDrawController: SelectionDrawController = new SelectionDrawController()
  val selectionsController: SelectionsController = new SelectionsController()

  // Canvas with pictures
  val bPane = new BorderPane()
  val canvasOverlay = new Canvas()

  // Drawing rectangles for selections
  var drawnRectangles: List[Rectangle] = List[Rectangle]()

  // Options
  val layersOptions: ObservableList[String] = FXCollections.observableArrayList()
  val layersComboBox = new ComboBox[String](layersOptions)
  val selectionsOptions: ObservableList[String] = FXCollections.observableArrayList("Full-size selection")

  // Logger
  val loggerField = new TextArea()

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
    for (_ <- 0 until pane.getChildren.size()) {
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
    bPane.setTop(this.createTopPane(primaryStage))
    bPane.setBottom(this.createBottomPane())
    bPane.setLeft(this.createLeftPane())
    bPane.setRight(this.createRightPane())

    canvasOverlay.setWidth(512)
    canvasOverlay.setHeight(512)

    canvasOverlay.setOnMousePressed((event: MouseEvent) => {
      selectionDrawController.onMousePressed(event)
    })
    canvasOverlay.setOnMouseDragged((event: MouseEvent) => {
      if (selectionDrawController.isUserDrawingSelections) {
        selectionDrawController.adjustRectProperties(event, canvasOverlay.getHeight, canvasOverlay.getWidth)
        clearDrawingCanvas()
        // Draw rectangles on screen (canvas)
        val gc = canvasOverlay.getGraphicsContext2D
        for (r <- drawnRectangles)  {
          val gc = canvasOverlay.getGraphicsContext2D
          gc.strokeRoundRect(r.topLeftCorner.x, r.topLeftCorner.y, r.dim.width, r.dim.height, 1, 1)
        }

        val newRect: JavaFXRectangle = selectionDrawController.rectangle
        gc.strokeRoundRect(newRect.getX, newRect.getY, newRect.getWidth, newRect.getHeight, 1, 1)
      }
    })
    canvasOverlay.setOnMouseReleased((event: MouseEvent) => {
      val rectToAdd: Rectangle = selectionDrawController.setOnMouseReleased(event) match {
        case Some(r) => r
        case _ => null
      }
      if (rectToAdd != null) drawnRectangles = rectToAdd :: drawnRectangles
    })

    root.getChildren.add(bPane)
    primaryStage.setScene(new Scene(root))
    primaryStage.show()
  }

  def createLeftPane(): VBox = {
    val leftPane = new VBox()

    val addLayerButton = new Button("Add layer...")

    // Add listener for creating new layer (opening picture)
    val event: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val fileChooser: FileChooser = new FileChooser()
        fileChooser.getExtensionFilters.add(new FileChooser.ExtensionFilter("PNG/JPG", "*.png", "*.jpg"))
        fileChooser.setTitle("Open picture...")
        fileChooser.setInitialDirectory(new File("pictures"))
        val stage: Stage = e.getSource.asInstanceOf[Node].getScene.getWindow.asInstanceOf[Stage]
        val file: File = fileChooser.showOpenDialog(stage)
        if (file != null) {
          val fileName: String = file.getName
          val newFileName: String = {
            // Enables layers to have the same picture, but different names
            if (layersController.findLayerByName(fileName).isDefined) {
              s"${fileName}_${layersController.countLayersWithSimilarNames(fileName)}"
            } else {
              fileName
            }
          }
          val picture: Picture = Utility.readPictureFromFile(file)
          val newLayer: Layer = new Layer(picture, newFileName)
          layersController.add_layer(newLayer)
          setNewCanvas(layersController.drawLayers())
          layersOptions.clear()
          for (layerName <- layersController.layersNames.reverse) layersOptions.add(layerName)
          updateLogger(s"New layer ${newLayer.name} added.", LoggerMessageType.INFO)
        }
      }
    }
    addLayerButton.setOnAction(event)

    // Layer name
    val activeLayer = new HBox()
    val activeLayerName = new Label("...")
    activeLayer.getChildren.addAll(new Label("Layer name: "), activeLayerName)
    activeLayer.setAlignment(Pos.CENTER)

    // Transparency
    val transparency = new HBox()
    transparency.setSpacing(5)
    val transparencyValueField = new TextField("")
    transparencyValueField.setMaxWidth(35)
    transparency.getChildren.addAll(new Label("Transparency: "), transparencyValueField)
    transparency.setAlignment(Pos.CENTER)

    // Is layer active
    val activeSelect = new HBox()
    activeSelect.setSpacing(5)
    val group = new ToggleGroup()
    val rb1 = new RadioButton("Yes")
    val rb2 = new RadioButton("No")
    rb1.setToggleGroup(group)
    rb2.setToggleGroup(group)
    activeSelect.getChildren.addAll(new Label("Active: "), rb1, rb2)
    activeSelect.setAlignment(Pos.CENTER)

    // Apply changes to layer
    val applyLayerChangesButton = new Button()
    applyLayerChangesButton.setText("Apply changes")
    val moveUpButton = new Button()
    // Move layer up
    moveUpButton.setText("Move up")
    // Move layer down
    val moveDownButton = new Button()
    moveDownButton.setText("Move down")

    // Add listener for layer selection change
    layersComboBox.valueProperty.addListener(new ChangeListener[String]() {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val layerOpt: Option[Layer] = layersController.findLayerByName(newValue)
        if (layerOpt.isDefined) {
          val layer: Layer = layerOpt match {case Some(l) => l}
          transparencyValueField.setText(layer.transparency.toString)
          activeLayerName.setText(layer.name)
          if (layer.active) rb1.setSelected(true)
          else rb2.setSelected(true)
          updateLogger(s"Layer ${layer.name} selected.", LoggerMessageType.INFO)
        }
      }
    })

    // Apply layer changes
    val applyLayerChangesEvent: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        try {
          val layerName: String = activeLayerName.getText
          val layerOpt: Option[Layer] = layersController.findLayerByName(layerName)
          if (layerOpt.isDefined) {
            val layerTransparency: Double = transparencyValueField.getText.toDouble
            val layer: Layer = layerOpt match {case Some(l) => l}
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
              updateLogger(s"Changes to layer ${layer.name} applied.", LoggerMessageType.INFO)
              setNewCanvas(layersController.drawLayers())
            } else {
              updateLogger(s"Transparency should be in range of 0 and 1. $layerTransparency found.", LoggerMessageType.ERROR)
            }
          } else {
            updateLogger(s"No layer selected.", LoggerMessageType.ERROR)
          }
        } catch {
          case _ : NumberFormatException =>
            updateLogger(s"Transparency value could not be parsed. Value should be type of double.", LoggerMessageType.ERROR)
        }
      }
    }
    applyLayerChangesButton.setOnAction(applyLayerChangesEvent)

    // Move layer backwards
    val moveLayerBackwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layerOpt: Option[Layer] = layersController.findLayerByName(layerName)
        if (layerOpt.isDefined) {
          val layer: Layer = layerOpt match {case Some(l) => l}
          layersController.moveLayerBackwards(layer)
          layersOptions.clear()
          for (layerName <- layersController.layersNames.reverse) layersOptions.add(layerName)
          layersComboBox.getSelectionModel.select(layerName)
          setNewCanvas(layersController.drawLayers())
          updateLogger(s"Layer ${layer.name} moved backwards.", LoggerMessageType.INFO)
        }
      }
    }
    moveDownButton.setOnAction(moveLayerBackwards)

    // Move layer forwards
    val moveLayerForwards: EventHandler[ActionEvent] = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val layerName: String = activeLayerName.getText
        val layerOpt: Option[Layer] = layersController.findLayerByName(layerName)
        if (layerOpt.isDefined) {
          val layer: Layer = layerOpt match {case Some(l) => l}
          layersController.moveLayerForwards(layer)
          layersOptions.clear()
          for (layerName <- layersController.layersNames.reverse) layersOptions.add(layerName)
          layersComboBox.getSelectionModel.select(layerName)
          setNewCanvas(layersController.drawLayers())
          updateLogger(s"Layer ${layer.name} moved forwards.", LoggerMessageType.INFO)
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
    val rightPane = new VBox()

    val addSelectionButton: Button = new Button("Add selection...")

    val selection: FlexibleSelection = new FlexibleSelection("Full-size selection")
    selectionsController.addSelection(selection)
    val selectionsComboBox: ComboBox[String] = new ComboBox[String](selectionsOptions)

    // Dialog for layer name
    val selectionNameDialog: TextInputDialog = new TextInputDialog
    selectionNameDialog.setHeaderText("Choose name for the new selection.")
    selectionNameDialog.setTitle("Selection name")
    selectionNameDialog.setGraphic(null)

    // Add new selection / Stop drawing event
    val selectionDrawingEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        if (layersController.activeLayers.isEmpty) return
        if (!selectionDrawController.isUserDrawingSelections) {
          selectionNameDialog.showAndWait
          val selectionName: String = selectionNameDialog.getEditor.getText
          if (selectionName == "") {
            updateLogger(s"Selection has to have name.", LoggerMessageType.ERROR)
            return
          }
          if (selectionsController.findSelectionByName(selectionName).isDefined) {
            updateLogger(s"Selection with the name $selectionName already exists. Please choose another one.", LoggerMessageType.ERROR)
            return
          }
          addSelectionButton.setText("Finish drawing")
          selectionDrawController.isUserDrawingSelections = true
          clearDrawingCanvas()
        } else {
          val selectionName: String = selectionNameDialog.getEditor.getText
          selectionsOptions.add(selectionName)
          val selection: Selection = Selection(selectionName, drawnRectangles)
          selectionsController.addSelection(selection)
          addSelectionButton.setText("Add selection...")
          selectionDrawController.isUserDrawingSelections = false
          drawnRectangles = List[Rectangle]()
          clearDrawingCanvas()
          updateLogger(s"New selection ${selection.name} added.", LoggerMessageType.INFO)
        }

      }
    }
    addSelectionButton.setOnAction(selectionDrawingEvent)

    // Selection name
    val activeSelection = new HBox()
    val activeSelectionName = new Label("...")
    activeSelection.getChildren.addAll(new Label("Selection name: "), activeSelectionName)

    // Change layer name event
    selectionsComboBox.valueProperty.addListener(new ChangeListener[String]() {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        val selectionOpt: Option[BaseSelection] = selectionsController.findSelectionByName(newValue)
        if (selectionOpt.isDefined) {
          val selection: BaseSelection = selectionOpt match {case Some(s) => s}
          activeSelectionName.setText(selection.name)
          updateLogger(s"Selection ${selection.name} selected.", LoggerMessageType.INFO)
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
      }
    })

    // Color picker for applying color
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

    // Show preview of chosen color
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
          } else {
            updateLogger(s"Color values should be in range of 0 and 1.", LoggerMessageType.ERROR)
          }

        } catch {
          case _ : NumberFormatException =>
            updateLogger(s"Color values could not be parsed. Values should be type of double.", LoggerMessageType.ERROR)
        }
      }
    }
    redTxtFld.textProperty().addListener(colorChangeListener)
    greenTxtFld.textProperty().addListener(colorChangeListener)
    blueTxtFld.textProperty().addListener(colorChangeListener)

    // Apply color
    val applyChangesButton = new Button()
    applyChangesButton.setText("Apply color")

    // Apply color event
    // Read color from color preview
    val colorPickerEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selectionOpt: Option[BaseSelection] = selectionsController.findSelectionByName(activeSelectionName.getText)
        if (selectionOpt.isDefined) {
          val selection: BaseSelection = selectionOpt match {case Some(s) => s}
          val color: Color = colorPreview.getFill.asInstanceOf[Color]
          selection.applyColor(layersController.layers, color.getRed, color.getGreen, color.getBlue)
          setNewCanvas(layersController.drawLayers())
          updateLogger(s"Color applied successfully.", LoggerMessageType.INFO)
        }
      }
    }
    applyChangesButton.setOnAction(colorPickerEvent)

    // Filtering
    val filterOptions: ObservableList[String] = FXCollections.observableArrayList(
      "Addition", "Subtraction", "Inverse subtraction",
      "Multiplication", "Division", "Inverse division",
      "Power", "Logarithm", "Absolute value",
      "Minimum", "Maximum", "Inversion",
      "Grayscale", "Median", "Sobel",
      "Sharpen", "Gaussian blur")
    val filtersComboBox = new ComboBox[String](filterOptions)

    val filterButton = new Button()
    filterButton.setText("Apply Filter")

    // Const input
    val filterConstTxtFld: TextField = new TextField("0.0")
    filterConstTxtFld.setMaxWidth(35)

    val filterWrapper: HBox = new HBox()
    filterWrapper.setSpacing(5)
    filterWrapper.getChildren.addAll(filtersComboBox, filterConstTxtFld)

    // Apply filtering event
    val filterEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selectionOpt: Option[BaseSelection] = selectionsController.findSelectionByName(activeSelectionName.getText)
        if (selectionOpt.isDefined) {
          val selection: BaseSelection = selectionOpt match {case Some(s) => s}
          val filterString: String = filtersComboBox.getValue

          def getValueFromTextField: Option[Double] = {
            try {
              val text: String = filterConstTxtFld.getText
              Some(text.toDouble)
            } catch {
              case _ : NumberFormatException => {
                updateLogger(s"Filtering const value could not be parsed. Value should be type of double.", LoggerMessageType.ERROR)
                None
              }
            }
          }

          val (success, errorMessage): (Boolean, Option[String]) = filterString match {
            case "Addition" if getValueFromTextField.isDefined
            => selection.add(getValueFromTextField.get, layersController.activeLayers)
            case "Subtraction" if getValueFromTextField.isDefined
            => selection.sub(getValueFromTextField.get, layersController.activeLayers)
            case "Inverse subtraction" if getValueFromTextField.isDefined
            => selection.inverseSub(getValueFromTextField.get, layersController.activeLayers)
            case "Multiplication" if getValueFromTextField.isDefined
            => selection.mul(getValueFromTextField.get, layersController.activeLayers)
            case "Division" if getValueFromTextField.isDefined
            => selection.div(getValueFromTextField.get, layersController.activeLayers)
            case "Inverse division" if getValueFromTextField.isDefined
            => selection.inverseDiv(getValueFromTextField.get, layersController.activeLayers)
            case "Power" if getValueFromTextField.isDefined
            => selection.pow(getValueFromTextField.get, layersController.activeLayers)
            case "Logarithm"
            => selection.log(layersController.layers)
            case "Absolute value"
            => selection.abs(layersController.layers)
            case "Minimum" if getValueFromTextField.isDefined
            => selection.min(getValueFromTextField.get, layersController.activeLayers)
            case "Maximum" if getValueFromTextField.isDefined
            => selection.max(getValueFromTextField.get, layersController.activeLayers)
            case "Inversion"
            => selection.inversion(layersController.activeLayers)
            case "Grayscale"
            => selection.grayscale(layersController.activeLayers)
            case "Median"
            => selection.median(layersController.activeLayers)
            case "Sobel"
            => selection.sobel(layersController.activeLayers)
            case "Sharpen"
            => selection.sharpen(layersController.activeLayers)
            case "Gaussian blur"
            => selection.gaussianBlur(layersController.activeLayers)
            case _ => (false, Some("Error during filtering."))
          }

          if (!success)
            updateLogger(errorMessage.get, LoggerMessageType.ERROR)
          else
            updateLogger(s"Filter applied successfully.", LoggerMessageType.INFO)

          setNewCanvas(layersController.drawLayers())
        }
      }
    }
    filterButton.setOnAction(filterEvent)

    // Delete selection
    val deleteSelectionButton = new Button()
    deleteSelectionButton.setText("Delete selection")

    val deleteSelectionEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val selectionName: String = activeSelectionName.getText
        val selectionOpt: Option[BaseSelection] = selectionsController.findSelectionByName(selectionName)
        if (selectionOpt.isDefined) {
          val selection: BaseSelection = selectionOpt match {case Some(s) => s}
          selection match {
            case _: Selection =>
              // Delete selection
              selection.restore(layersController.layers)
              selectionsController.removeSelection(selection)
              selectionsOptions.remove(selectionName)
              setNewCanvas(layersController.drawLayers())
              updateLogger(s"Selection ${selection.name} deleted.", LoggerMessageType.INFO)
            case _: FlexibleSelection =>
              // Restore selection, but don't delete it
              selection.restore(layersController.layers)
              setNewCanvas(layersController.drawLayers())
              updateLogger(s"Changes applied to the whole images reverted.", LoggerMessageType.INFO)
            case _ =>
          }
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

  def createTopPane(primaryStage: Stage): HBox = {
    val topPane = new HBox()

    val loadProjectButton = new Button()
    loadProjectButton.setText("Load...")
    val saveProjectButton = new Button()
    saveProjectButton.setText("Save As...")
    val exportPicButton = new Button()
    exportPicButton.setText("Export picture")

    val exportPicEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        val fileChooser = new FileChooser
        fileChooser.setInitialDirectory(new File("pictures"))
        fileChooser.getExtensionFilters.addAll(new FileChooser.ExtensionFilter("PNG", "*.png"),
                                               new FileChooser.ExtensionFilter("JPG", "*.jpg"))
        fileChooser.setTitle("Save picture")
        val file: File = fileChooser.showSaveDialog(primaryStage)
        if (file != null) try {
          val dimsList = for (layer <- layersController.activeLayers.reverse) yield layer.picture.dim
          val maxDim = layersController.findMaxDimension(dimsList)
          val canvasesForExport = layersController.drawLayers()
          val writableImage = new WritableImage(maxDim.width, maxDim.height)
          canvasesForExport.snapshot(null, writableImage)
          val renderedImage = SwingFXUtils.fromFXImage(writableImage, null)
          ImageIO.write(renderedImage, "png", file)
          updateLogger(s"Image exported.", LoggerMessageType.INFO)
        } catch {
          case ex: IOException =>
            ex.printStackTrace()
        }
      }
    }
    exportPicButton.setOnAction(exportPicEvent)

    val saveProjectEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        def serialize(value: Any): String = {
          val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
          val oos = new ObjectOutputStream(stream)
          oos.writeObject(value)
          oos.close()
          new String(Base64.getEncoder.encode(stream.toByteArray), UTF_8)
        }

        val fileChooser = new FileChooser()
        fileChooser.setInitialDirectory(new File("pictures"))
        fileChooser.getExtensionFilters.add(new FileChooser.ExtensionFilter("proj", "*.proj"))
        fileChooser.setTitle("Save project")
        val file = fileChooser.showSaveDialog(primaryStage)
        if (file != null) try {
          Using(new PrintWriter(file)) {
            pw =>
              pw.write(serialize(layersController.layers))
              pw.write("\n")
              pw.write(serialize(selectionsController.selections))
          }
        } catch {
          case e: Throwable => updateLogger(e.getMessage, LoggerMessageType.ERROR)
        }
      }
    }
    saveProjectButton.setOnAction(saveProjectEvent)

    val loadProjectEvent = new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        def deserialize(str: String): AnyRef = {
          val bytes = Base64.getDecoder.decode(str.getBytes(UTF_8))
          Using(new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            ois =>
              val value = ois.readObject
              return value
          }
        }

        val fileChooser = new FileChooser()
        fileChooser.setInitialDirectory(new File("pictures"))
        fileChooser.getExtensionFilters.add(new FileChooser.ExtensionFilter("proj", "*.proj"))
        fileChooser.setTitle("Load project")
        val file: File = fileChooser.showOpenDialog(primaryStage)
        if (file != null) {
          val source: BufferedSource = Source.fromFile(file)
          try {
            var index: Int = 0
            for (line <- source.getLines()) {
              index match {
                case 0 => {
                  layersController.layers = deserialize(line).asInstanceOf[List[Layer]]
                  layersOptions.clear()
                  for (name <- layersController.layersNames) layersOptions.add(name)
                }
                case 1 => {
                  selectionsController.selections = deserialize(line).asInstanceOf[List[Selection]]
                  selectionsOptions.clear()
                  for (name <- selectionsController.selectionsNames) selectionsOptions.add(name)
                }
              }
              index = index + 1
            }
            updateLogger(s"Project loaded.", LoggerMessageType.INFO)
          } catch {
            case e: Throwable => updateLogger(s"Error while trying to import project. ${e.getMessage}", LoggerMessageType.ERROR)
          }
          source.close()
          setNewCanvas(layersController.drawLayers())
        }
      }
    }
    loadProjectButton.setOnAction(loadProjectEvent)

    HBox.setMargin(loadProjectButton, new Insets(20, 20, 20, 20))
    HBox.setMargin(saveProjectButton, new Insets(20, 20, 20, 0))
    HBox.setMargin(exportPicButton, new Insets(20, 20, 20, 0))

    topPane.setAlignment(Pos.CENTER_LEFT)

    topPane.getChildren.addAll(loadProjectButton, saveProjectButton, exportPicButton)

    topPane
  }

  def createBottomPane(): VBox = {
    val bottomPane: VBox = new VBox()
    loggerField.setEditable(false)
    bottomPane.getChildren.addAll(loggerField)
    bottomPane
  }

  def updateLogger(text: String, loggerMessageType: LoggerMessageType): Unit = {
    loggerField.appendText(s"${LocalTime.now()} [$loggerMessageType]: $text\n")
  }
}
