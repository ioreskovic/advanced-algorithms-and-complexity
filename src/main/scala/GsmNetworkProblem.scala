import scala.io.StdIn

object GsmNetworkProblem {
  trait Term {
    def sign: Boolean
    def label: String
    def unary_-(): Term

    override lazy val toString: String = sign match {
      case true => label
      case false => s"-$label"
    }
  }

  case class PositiveTerm(label: String) extends Term {
    def sign: Boolean = true
    def unary_-(): NegativeTerm = NegativeTerm(label)
  }

  case class NegativeTerm(label: String) extends Term {
    def sign: Boolean = false
    def unary_-(): PositiveTerm = PositiveTerm(label)
  }

  case class CNF(ds: Seq[Term]) {
    override def toString: String = {
      ds.mkString("", " ", " 0")
    }
  }

  case class Edge(a: Int, b: Int) {
  }

  object Edge {
    def apply(s: String): Edge = {
      val info = s.split(" ")
      Edge(info(0).toInt - 1, info(1).toInt - 1)
    }
  }

  case class CellGraphInfo(nVertices: Int, nColors: Int) {
    def labelFor(vertexIdx: Int, colorIdx: Int): String = (1 + colorIdx + vertexIdx * nColors).toString
    lazy val vertices: Seq[Int] = (0 until nVertices).toList
    lazy val colors: Seq[Int] = (0 until nColors).toList
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nVertices = metaInfo(0).toInt
    val nEdges = metaInfo(1).toInt
    val nColors = 3
    val cellGraphInfo = CellGraphInfo(nVertices, nColors)

    val verticesContraints = cellGraphInfo.vertices.flatMap(v => {
      val notUncolored = CNF(cellGraphInfo.colors.map(c => PositiveTerm(cellGraphInfo.labelFor(v, c))))
      val notMulticolored = for {
        c1 <- 0 until cellGraphInfo.nColors - 1
        c2 <- (c1 + 1) until cellGraphInfo.nColors
      } yield CNF(Seq(NegativeTerm(cellGraphInfo.labelFor(v, c1)), NegativeTerm(cellGraphInfo.labelFor(v, c2))))
      notUncolored :: notMulticolored.toList
    })

    val edgesConstraints = (0 until nEdges).flatMap(_ => {
      val e = Edge(StdIn.readLine())
      cellGraphInfo.colors.map(c => {
        CNF(Seq(NegativeTerm(cellGraphInfo.labelFor(e.a, c)), NegativeTerm(cellGraphInfo.labelFor(e.b, c))))
      })
    })

    val nClauses = verticesContraints.length + edgesConstraints.length
    val nVariables = cellGraphInfo.nColors * cellGraphInfo.nVertices
    println(s"$nClauses $nVariables")
    verticesContraints.foreach(println)
    edgesConstraints.foreach(println)
  }
}
