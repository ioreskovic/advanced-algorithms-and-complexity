package week4.wip

import week4.wip.Graph.Edge

class DirectedGraph[Vertex](val vertices: List[Vertex], val adj: Map[Vertex, List[Vertex]])(implicit conversion: Vertex => Int) extends Graph[Vertex] {
  override def transpose: DirectedGraph[Vertex] = {
    import scala.collection.mutable.{ HashMap => MutableHashMap }

    val transposedAdj = MutableHashMap[Vertex, List[Vertex]]().withDefaultValue(Nil)

    for ((fromVertex, toVertices) <- adj) {
      for (toVertex <- toVertices) {
        transposedAdj(toVertex) = fromVertex :: transposedAdj(toVertex)
      }
    }

    new DirectedGraph(vertices, transposedAdj.toMap.withDefaultValue(Nil))
  }

  override lazy val toString: String = {
    adj.mkString(s"Graph{$vertices", "\n", "\n")
  }
}

object DirectedGraph {
  def apply[Vertex](vertices: Seq[Vertex], edges: Seq[Edge[Vertex]])(implicit conversion: Vertex => Int): DirectedGraph[Vertex] = {
    import scala.collection.mutable.{ HashMap => MutableHashMap }
    val adjMap = MutableHashMap[Vertex, List[Vertex]]().withDefaultValue(Nil)

    for (e <- edges) {
      adjMap(e.from) = e.to :: adjMap(e.from)
    }

    new DirectedGraph(vertices.toList, adjMap.toMap.withDefaultValue(Nil))
  }

  def apply[Vertex](edges: Seq[Edge[Vertex]])(implicit conversion: Vertex => Int): DirectedGraph[Vertex] = {
    import scala.collection.mutable.{ HashSet => MutableHashSet, HashMap => MutableHashMap }
    val vertexSet = MutableHashSet[Vertex]()
    val adjMap = MutableHashMap[Vertex, List[Vertex]]().withDefaultValue(Nil)

    for (e <- edges) {
      vertexSet(e.from) = true
      vertexSet(e.to) = true
      adjMap(e.from) = e.to :: adjMap(e.from)
    }

    new DirectedGraph(vertexSet.toList, adjMap.toMap.withDefaultValue(Nil))
  }
}
