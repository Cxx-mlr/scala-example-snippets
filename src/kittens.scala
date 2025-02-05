class Kitten(val name: String, val age: Int, val weight: Double, val health: String) {
  def isHealthy: Boolean = health == "good"
}

def processKittens(kittens: List[Kitten], filter: Kitten => Boolean, transform: Kitten => Kitten): List[Kitten] = {
  kittens.filter(filter).map(transform)
}

def countHealthyKittens(kittens: List[Kitten]): Int = kittens match {
  case Nil => 0
  case head :: tail => (if (head.isHealthy) 1 else 0) + countHealthyKittens(tail)
}

def sortKittens(kittens: List[Kitten], sortBy: Kitten => Double): List[Kitten] = {
  kittens.sortBy(sortBy)
}

def sumAges(kittens: List[Kitten]): Int = {
  @annotation.tailrec
  def sumAgesTailRec(kittens: List[Kitten], accumulator: Int): Int = kittens match {
    case Nil => accumulator
    case head :: tail => sumAgesTailRec(tail, accumulator + head.age)
  }
  sumAgesTailRec(kittens, 0)
}

def namesAndWeightsHealthyKittens(kittens: List[Kitten]): List[(String, Double)] = {
  kittens.filter(_.isHealthy).map(k => (k.name, k.weight))
}

object KittenApp {
  def main(args: Array[String]): Unit = {
    val kittens = List(
      new Kitten("Tom", 14, 4.0, "bad"),
      new Kitten("Jerry", 2, 1.0, "good"),
      new Kitten("Misaki", 6, 3.0, "good"),
      new Kitten("Katty", 2, 2.0, "good"),
    )

    println(s"Edad total de los gatitos: ${sumAges(kittens)}")
    println(s"Número de gatitos saludables: ${countHealthyKittens(kittens)}")
    println("Nombres y pesos de los gatitos saludables en orden ascendente (peso):")
    namesAndWeightsHealthyKittens(sortKittens(kittens, _.weight)).foreach(
      (k, v) => println(s"- ${k} ${v}kg")
    )

    val processedKittens1 = processKittens(kittens, _.isHealthy, k => new Kitten(k.name, k.age, k.weight + 0.5, k.health))
    println(s"\nDespués de procesar gatitos saludables (peso + 0.5):")
    namesAndWeightsHealthyKittens(sortKittens(processedKittens1, _.weight)).foreach(
      (k, v) => println(s"- ${k} ${v}kg")
    )

    val processedKittens2 = processKittens(kittens, _.age > 2, k => new Kitten(k.name, k.age, k.weight - 0.2, k.health))
    println(s"\nDespués de procesar gatitos mayores de 2 años (peso - 0.2)")
    sortKittens(processedKittens2, _.weight).foreach(
      k => println(s"- ${k.name} ${k.weight}kg")
    )

    println("\nUso de la función namesAndWeightsHealthyKittens para obtener una lista de tuplas de los nombres y pesos de los gatitos saludables:")
    namesAndWeightsHealthyKittens(kittens).foreach(
      t => println(s"- ${t}")
    )
  }
}