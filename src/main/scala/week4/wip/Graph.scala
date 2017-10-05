package week4.wip

import scala.collection.mutable.ListBuffer

abstract class Graph[Vertex](implicit conversion: Vertex => Int) {
  def transpose: Graph[Vertex]

  def apply(vertex: Vertex): List[Vertex] = adj(vertex)

  def adj: Vertex => List[Vertex]

  def postOrder: List[Vertex] = {
    import scala.collection.mutable.{HashSet => MutableHashSet}

    val order = ListBuffer[Vertex]()
    val visited = MutableHashSet[Int]()

    def dfsLoop(vertex: Vertex): Unit = {
      if (!visited(vertex)) {
        visited(vertex) = true
        adj(vertex).foreach(neighbour => dfsLoop(neighbour))
        order.append(vertex)
      }
    }

    for (vertex <- vertices) {
      dfsLoop(vertex)
    }

    order.toList
  }

  def nVertices: Int = vertices.length

  def vertices: List[Vertex]
}

object Graph {

  case class Edge[Vertex](from: Vertex, to: Vertex)

}
