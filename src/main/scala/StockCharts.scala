import scala.io.StdIn

object StockCharts {
  case class Stock(prices: List[Int]) {
    def <(other: Stock): Boolean = this.prices.zip(other.prices).forall{ case (thisPrice, otherPrice) => thisPrice < otherPrice }
  }

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
    val stocks = (0 until rows).map(_ => parseStock(StdIn.readLine()))
    val graph = toBipartiteGraph(stocks)
    val matching = findMatching(graph)
    println(stocks.size - matching)
  }

  def parseStock(string: String): Stock = {
    Stock(string.split(" ").map(_.toInt).toList)
  }

  def toBipartiteGraph(stocks: Seq[Stock]): AdjMatrixGraph = {
    val graph = AdjMatrixGraph(stocks.size, stocks.size)
    val indexedStocks = stocks.zipWithIndex
    for ((s1, i) <- indexedStocks) {
      for ((s2, j) <- indexedStocks) {
        if (i != j && s1 < s2) graph.connect(i, j)
      }
    }
    graph
  }

  def findMatching(graph: AdjMatrixGraph): Int = {
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

    (0 until graph.rows).foldLeft(0){ case (paths, row) =>
        val hasPath = loop(Some(row), MHashSet())
        if (hasPath) paths + 1
        else paths
    }
  }
}
