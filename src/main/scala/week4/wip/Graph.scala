package week4.wip

import scala.annotation.tailrec
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

  def scc: List[StronglyConnectedComponent[Vertex]] = {
    import scala.collection.mutable.{HashMap => MutableHashMap, HashSet => MutableHashSet}
    val reversePostOrder = postOrder.reverse
    val sccMap = MutableHashMap[Int, List[Vertex]]().withDefaultValue(Nil)
    val t = transpose
    val visited = MutableHashSet[Int]()

    def explore(vertex: Vertex, sccIndex: Int): Unit = {
      visited(vertex) = true
      sccMap.update(sccIndex, vertex :: sccMap(sccIndex))
      t(vertex).foreach(n => if (!visited(n)) explore(n, sccIndex))
    }

    @tailrec
    def loop(vx: List[Vertex], sccIndex: Int = 0): Unit = vx match {
      case v :: vs if !visited(v) => {
        explore(v, sccIndex)
        loop(vs, sccIndex + 1)
      }
      case _ :: vs => loop(vs, sccIndex)
      case Nil =>
    }

    loop(reversePostOrder)

    sccMap.map{ case (id, vertices) => StronglyConnectedComponent(id, vertices)}.toList
  }

  def nVertices: Int = vertices.length

  def vertices: List[Vertex]
}

object Graph {

  case class Edge[Vertex](from: Vertex, to: Vertex)

}
