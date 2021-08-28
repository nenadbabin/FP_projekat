package controller

import selection.{BaseSelection, Selection}
import utility.Rectangle

import scala.annotation.tailrec

class SelectionsController {

  var selections: List[BaseSelection] = List()

  def addSelection(newSelection: BaseSelection): Unit = {
    def addSelectionToList(newSelection: BaseSelection): List[BaseSelection] = selections match {
      case List() => List(newSelection)
      case _ => newSelection :: selections
    }

    selections = addSelectionToList(newSelection)
  }

  def removeSelection(selectionToRemove: BaseSelection): Unit = {
    selections = selections.filter(_ != selectionToRemove)
  }

  def findSelectionByName(name: String): BaseSelection = {
    @tailrec
    def find(list: List[BaseSelection]): BaseSelection = list match {
      case List() => null
      case h::t =>
        if (h.name == name) h
        else find(t)
    }
    find(selections)
  }

  def addRectangleToSelection(rect: Rectangle, selection: Selection): Selection = {
    val rectList: List[Rectangle] = selection.rectangles
    val newRectList: List[Rectangle] = rectList ::: List[Rectangle](rect)
    selection.copy(rectangles = newRectList)
  }

}
