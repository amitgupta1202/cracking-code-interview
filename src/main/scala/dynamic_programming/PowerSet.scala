package dynamic_programming

object PowerSet extends App {

  def allSubsets(elem: List[Int]): List[List[Int]] =
    elem match {
      case Nil => List(List())
      case head :: tail =>
        val subsets = allSubsets(tail)
        val subsetsToAdd = subsets.map(s => s :+ head)
        subsets ++ subsetsToAdd
    }

  println(allSubsets(List(1, 2, 3)))

}
