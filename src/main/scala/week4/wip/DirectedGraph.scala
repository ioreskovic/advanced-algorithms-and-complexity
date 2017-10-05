package week4.wip

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
}
