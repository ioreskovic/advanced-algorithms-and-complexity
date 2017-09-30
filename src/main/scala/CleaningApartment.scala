import scala.io.StdIn

object CleaningApartment {
  trait Term {
    def sign: Boolean
    def label: String
    def unary_-(): Term

    override lazy val toString: String = if (sign) {
      label
    } else {
      s"-$label"
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

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nVertices = metaInfo(0).toInt
    val nEdges = metaInfo(1).toInt
    val edges = (0 until nEdges).foldLeft(Set[(Int, Int)]()){ case (m, _) =>
      val e = Edge(StdIn.readLine())
      m + ((e.a, e.b), (e.b, e.a))
    }

    def labelFor(vertex: Int, position: Int): String = {
      (1 + vertex + position * nVertices).toString
    }

    val range = 0 until nVertices

    val eachVertexAtLeastOnceOnPath = range.map(j => {
      CNF(range.map(i => PositiveTerm(labelFor(j, i))))
    })

    val noVertexMoreThanOnceOnPath = range.flatMap(j => {
      for {
        i <- range
        k <- range
        if i < k
      } yield CNF(Seq(NegativeTerm(labelFor(j, i)), NegativeTerm(labelFor(j, k))))
    })

    val eachPositionOnPathOccupiedAtLeastOnce = range.map(i => {
      CNF(range.map(j => PositiveTerm(labelFor(j, i))))
    })

    val noDifferentVerticesOccupySamePathPosition = range.flatMap(i => {
      for {
        j <- range
        k <- range
        if j < k
      } yield CNF(Seq(NegativeTerm(labelFor(j, i)), NegativeTerm(labelFor(k, i))))
    })

    val nonAdjacentVerticesCannotBeAdjacentInPath = (for {
      i <- 0 until nVertices
      j <- i + 1 until nVertices
      if !edges(i, j)
      k <- 0 until nVertices - 1
    } yield Seq(CNF(Seq(NegativeTerm(labelFor(k, i)), NegativeTerm(labelFor(k + 1, j)))), CNF(Seq(NegativeTerm(labelFor(k, j)), NegativeTerm(labelFor(k + 1, i)))))).flatten

    val nClauses = eachVertexAtLeastOnceOnPath.length + noVertexMoreThanOnceOnPath.length + eachPositionOnPathOccupiedAtLeastOnce.length + noDifferentVerticesOccupySamePathPosition.length + nonAdjacentVerticesCannotBeAdjacentInPath.length
    val nVariables = nVertices * nVertices

    println(s"$nClauses $nVariables")
    eachVertexAtLeastOnceOnPath.foreach(println)
    noVertexMoreThanOnceOnPath.foreach(println)
    eachPositionOnPathOccupiedAtLeastOnce.foreach(println)
    noDifferentVerticesOccupySamePathPosition.foreach(println)
    nonAdjacentVerticesCannotBeAdjacentInPath.foreach(println)
  }
}
