object ListProcessing {
  class ListProcessor {
    def processList(list: List[Int], f: Function[Int, Int]): List[Int] = {
      list.map(f)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val f = (x: Int) => (x + 10) * 2

    val listProcessor = new ListProcessor
    val rlist = listProcessor.processList(list, f)

    println(s"Original List: $list")
    println(s"Processed List: $rlist")
  }
}