object MinMax {
  def min(values: List[Int]): Int = values match {
    case Nil => throw new IllegalArgumentException("List is empty")
    case x :: Nil => x
    case x :: xs => val minTail = min(xs); if (x < minTail) x else minTail
  }

  def max(values: List[Int]): Int = values match {
    case Nil => throw new IllegalArgumentException("List is empty")
    case x :: Nil => x
    case x :: xs => val maxTail = max(xs); if (x > maxTail) x else maxTail
  }

  def main(args: Array[String]): Unit = {
    val numbers = List(11, 2, 7, 5, 3)
    println(s"Min: ${min(numbers)}")
    println(s"Max: ${max(numbers)}")
  }
}