package dynamic_programming

import scala.collection.mutable
/*
 A child is running up a staircase with n steps and can hop either 1 step, 2 steps, or 3
steps at a time. Implement a method to count how many possible ways the child can run up the
stairs.
 */
object TripleStep extends App {

  val cache: mutable.Map[Int, BigInt] = new mutable.HashMap[Int, BigInt]()

  def countWays(steps: Int): BigInt =
    if (steps < 0) 0
    else if (steps == 0) 1
    else {
      val maybeResult = cache.get(steps)
      if (maybeResult.isEmpty) {
        val result = countWays(steps - 1) + countWays(steps - 2) + countWays(steps - 3)
        cache.put(steps, result)
        result
      } else maybeResult.get
    }

  assert(countWays(500) == BigInt("1306186569702186634983475450062372018715120191391192207156664343051610913971927959744519676992404852130396504615663042713312314219527"))

}
