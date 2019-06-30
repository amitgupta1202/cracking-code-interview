package dynamic_programming

object TowerOfHanoi extends App {

  def solve(n: Int, from: String, via: String, to: String): Unit = {
    if (n == 1) println(s"move 1 from $from to $to")
    else {
      solve(n - 1, from, to, via)
      println(s"move $n from $from to $to")
      solve(n - 1, via, from, to)
    }
  }

  solve(3, "A", "B", "C")

}

