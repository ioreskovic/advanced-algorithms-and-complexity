import scala.io.StdIn

object BipartiteMatching {
  case class AdjMatrixGraph(rows: Int, cols: Int) {
    private val m: Array[Array[Boolean]] = Array.fill(rows, cols)(false)

    def connect(row: Int, col: Int): AdjMatrixGraph = {
      m(row)(col) = true
      this
    }

    def disconnect(row: Int, col: Int): AdjMatrixGraph = {
      m(row)(col) = false
      this
    }

    def update(row: Int, col: Int, hasConnection: Boolean): AdjMatrixGraph = {
      m(row)(col) = hasConnection
      this
    }

    def apply(row: Int)(col: Int): Boolean = m(row)(col)
  }

  def main(args: Array[String]): Unit = {
    val dims = StdIn.readLine().split(" ").map(_.toInt)
    val rows = dims(0)
    val cols = dims(1)
    val graph = (0 until rows).foldLeft(AdjMatrixGraph(rows, cols)) { case (g, row) =>
      StdIn.readLine().split(" ").zipWithIndex.foldLeft(g){
        case (h, ("1", col)) => h.connect(row, col)
        case (h, ("0", col)) => h.disconnect(row, col)
        case _ => throw new IllegalArgumentException
      }
    }
    val matching = findMatching(graph)
    val printableMatching = (0 until rows).map(row => matching.get(row) match {
      case None => -1
      case Some(col) => col + 1
    })
    println(printableMatching.mkString(" "))
  }

  def findMatching(graph: AdjMatrixGraph): Map[Int, Int] = {
    import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, Set => MSet, HashSet => MHashSet}
    val rowsMatching = MHashMap[Int, Int]()
    val colsMatching = MHashMap[Int, Int]()

    def loop(maybeX: Option[Int], visited: MHashSet[Int]): Boolean = maybeX match {
      case None => true
      case Some(x) if visited(x) => false
      case Some(row) => {
        visited(row) = true
        (0 until graph.cols).find(col => graph(row)(col) && loop(colsMatching.get(col), visited)) match {
          case None => false
          case Some(col) => {
            rowsMatching(row) = col
            colsMatching(col) = row
            true
          }
        }
      }
    }

    (0 until graph.rows).foreach(row => loop(Some(row), MHashSet()))
    rowsMatching.toMap
  }
}
