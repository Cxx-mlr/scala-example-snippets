object MinDistance {
  def minDistance(points: List[(Double, Double)], distance: ((Double, Double), (Double, Double)) => Double): Double = {
    def pairwiseDistances(ps: List[(Double, Double)]): List[Double] = ps match {
      case Nil => Nil
      case p :: ps1 => ps1.map(distance(p, _)) ++ pairwiseDistances(ps1)
    }

    pairwiseDistances(points).min
  }

  def euclideanDistance(p1: (Double, Double), p2: (Double, Double)): Double = {
    math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))
  }

  def main(args: Array[String]): Unit = {
    val points = List((1.0, 2.0), (4.0, 6.0), (5.0, 1.0), (2.0, 3.0))
    val minDist = minDistance(points, euclideanDistance)
    println(s"La distancia mínima es: $minDist")
  }
}