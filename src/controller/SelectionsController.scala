package controller

import selection.{BaseSelection, Selection}
import utility.Rectangle

import scala.annotation.tailrec

class SelectionsController {

  var selections: List[BaseSelection] = List()

  def selectionsNames: List[String] = for (selection <- selections.reverse) yield selection.name

  def addSelection(newSelection: BaseSelection): Unit = {
    def addSelectionToList(newSelection: BaseSelection): List[BaseSelection] = selections match {
      case List() => List(newSelection)
      case _ => newSelection :: selections
    }

    selections = addSelectionToList(newSelection)
  }

  def removeSelection(selectionToRemove: BaseSelection): Unit = {
    selections = selections.filter(_.name != selectionToRemove.name)
  }

  def findSelectionByName(name: String): Option[BaseSelection] = {
    @tailrec
    def find(list: List[BaseSelection]): Option[BaseSelection] = list match {
      case List() => None
      case h::t =>
        if (h.name == name) Some(h)
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
