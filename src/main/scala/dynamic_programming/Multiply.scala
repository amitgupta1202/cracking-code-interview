package dynamic_programming

object Multiply extends App {

  def multiply(num1: Int, num2: Int): Int = {
    val (smaller, bigger) = if (num1 < num2) (num1, num2) else (num2, num1)

    def helper(s: Int, b: Int): Int = {
      if (s == 0) 0
      else if (s == 1) b
      else {
        val temp = helper(s >> 1, b)
        if (s % 2 == 0) temp << 1 //or temp + temp
        else (temp << 1) + b
      }
    }

    helper(smaller, bigger)
  }

  println(multiply(100, 99))
}


