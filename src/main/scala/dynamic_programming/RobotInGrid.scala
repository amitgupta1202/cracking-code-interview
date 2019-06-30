package dynamic_programming

import scala.collection.mutable

/*
Imagine a robot sitting on the upper left corner of grid with r rows and c columns.
The robot can only move in two directions, right and down, but certain cells are "off limits" such that
the robot cannot step on them. Design an algorithm to find a path for the robot from the top left to
the bottom right.

- - - -
- - - -
- - - -
- - - -

001 001 001 001 001
001 002 003 004 005
001 003 006 010 015
001 004 010 020 035
001 005 015 035 070
 */


object RobotInGrid extends App {

  def noOfPath(row: Int, col: Int): Int =
    if (row == 1 || col == 1) 1
    else noOfPath(row - 1, col) + noOfPath(row, col - 1)


  case class Cell(r: Int, c: Int) {
    def up = Cell(r, c - 1)

    def left = Cell(r - 1, c)
  }

  def noOfPath2(row: Int, col: Int, notAllowed: Set[Cell] = Set.empty): Int = {
    val grid: mutable.Map[Cell, Int] = new mutable.HashMap()

    def find(cell: Cell): Int =
      if (cell.c <= 1 || cell.r <= 1) 1
      else grid(cell)

    (1 to row).foreach { r =>
      (1 to col).foreach { c =>
        val cell = Cell(r, c)
        if (notAllowed.contains(cell)) grid.put(cell, 0)
        else grid.put(cell, find(cell.up) + find(cell.left))
      }
    }

    grid(Cell(row, col))
  }


  println(noOfPath(3, 3))
  println(noOfPath(4, 4))
  println(noOfPath(5, 5))

  println(noOfPath2(3, 3))
  println(noOfPath2(4, 4, Set(Cell(3, 2))))
  println(noOfPath2(5, 5))
}
