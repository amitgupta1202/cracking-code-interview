package dynamic_programming

import scala.collection.mutable

object MaxSubSequence extends App {

  def solve(str1: List[Char], str2: List[Char]): Int =
    if (str1.isEmpty || str2.isEmpty) 0
    else if (str1.head == str2.head) 1 + solve(str1.tail, str2.tail)
    else Math.max(solve(str1, str2.tail), solve(str1.tail, str2))

  def solve2(str1: List[Char], str2: List[Char]): Int = {
    case class Cell(r: Int, c: Int)
    val table: mutable.Map[Cell, Int] = new mutable.HashMap()
    (0 to str1.length).foreach { r =>
      (0 to str2.length).foreach { c =>
        table.put(
          Cell(r, c),
          if (r == 0 || c == 0) 0
          else if (str1(r - 1) == str2(c - 1)) 1 + table(Cell(r - 1, c - 1))
          else Math.max(table(Cell(r - 1, c)), table(Cell(r, c - 1)))
        )
      }
    }
    table(Cell(str1.length, str2.length))
  }

  println(solve2("AGGTAB".toList, "GXTXAYB".toList))

}
