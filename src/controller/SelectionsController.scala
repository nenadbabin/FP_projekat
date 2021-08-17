package controller

import selection.Selection
import utility.Rectangle

import scala.annotation.tailrec

class SelectionsController {

  var selections: List[Selection] = List()

  def addSelection(newSelection: Selection): Unit = {
    def addSelectionToList(newSelection: Selection): List[Selection] = selections match {
      case List() => List(newSelection)
      case _ => List[Selection](newSelection) ::: selections
    }

    selections = addSelectionToList(newSelection)
  }

  def findSelectionByName(name: String): Selection = {
    @tailrec
    def find(list: List[Selection]): Selection = list match {
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
