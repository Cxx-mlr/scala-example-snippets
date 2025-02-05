object LongestWord {
  def main(args: Array[String]): Unit = {
    val sentences = List(
      "The quick brown fox jumps over the lazy dog",
      "Scala is a great programming language",
      "Functional programming is powerful"
    )

    val longestWord = sentences
      .flatMap(_.split(" "))
      .reduceLeft((a, b) => if (a.length > b.length) a else b)

    println(s"The longest word is: $longestWord")
  }
}