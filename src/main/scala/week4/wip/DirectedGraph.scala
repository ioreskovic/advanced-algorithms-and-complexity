package week4.wip

import week4.wip.Graph.Edge

case class DirectedGraph[Vertex](vertices: List[Vertex], adj: Map[Vertex, List[Vertex]])(implicit conversion: Vertex => Int) extends Graph[Vertex] {
  override def transpose: DirectedGraph[Vertex] = {
    import scala.collection.mutable.{ HashMap => MutableHashMap }

    val transposedAdj = MutableHashMap[Vertex, List[Vertex]]().withDefaultValue(Nil)

    for ((fromVertex, toVertices) <- adj) {
      for (toVertex <- toVertices) {
        transposedAdj(toVertex) = fromVertex :: transposedAdj(toVertex)
      }
    }

    DirectedGraph(vertices, transposedAdj.toMap)
  }
}

object DirectedGraph {
  def apply[Vertex](edges: List[Edge[Vertex]])(implicit conversion: Vertex => Int): DirectedGraph[Vertex] = {
    import scala.collection.mutable.{ HashSet => MutableHashSet, HashMap => MutableHashMap }
    val vertexSet = MutableHashSet[Vertex]()
    val adjMap = MutableHashMap[Vertex, List[Vertex]]().withDefaultValue(Nil)

    for (e <- edges) {
      vertexSet(e.from) = true
      vertexSet(e.to) = true
      adjMap(e.from) = e.to :: adjMap(e.from)
    }

    DirectedGraph(vertexSet.toList, adjMap.toMap)
  }
}
