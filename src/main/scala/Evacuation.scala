import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.StdIn

object Evacuation {
  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nVertices = metaInfo(0).toInt
    val nEdges = metaInfo(1).toInt
    val edges = (0 until nEdges).map(i => parseEdge(StdIn.readLine()))
    val graph = FlowGraph(edges: _*)
    val flow = maxflow(graph)
    println(flow)
  }

  def parseEdge(str: String): FlowEdge = {
    val info = str.split(" ")
    FlowEdge(info(0).toInt - 1, info(1).toInt - 1, info(2).toInt)
  }

  def maxflow(graph: FlowGraph): Int = {
    val path = augmentingPath(graph, 0, 4)
    println(path)
    0
  }

  def augmentingPath(graph: FlowGraph, from: Int, to: Int): List[Int] = {
    @tailrec
    def expand(q: Queue[Int], prev: Map[Int, Int], edges: List[FlowEdge], curr: Int): (Queue[Int], Map[Int, Int]) = edges match {
      case e :: es => {
        val newQ = q.enqueue(e.to)
        val newP = prev + (e.to -> curr)
        if (e.to == to) (newQ, newP)
        else expand(newQ, newP, es, curr)
      }
      case _ => (q, prev)
    }

    @tailrec
    def reconstructPath(from: Int, to: Int, p: Map[Int, Int], res: List[Int] = Nil): List[Int] = p.get(to) match {
      case None if from == to =>  from :: res
      case None => Nil
      case Some(prev) => reconstructPath(from, prev, p, to :: res)
    }

    @tailrec
    def loop(q: Queue[Int], visited: Set[Int] = Set(), prev: Map[Int, Int] = Map()): List[Int] = {
      if (q.isEmpty) {
        reconstructPath(from, to, prev)
      } else {
        val (currentEdgeId, tail) = q.dequeue
        if (!visited(currentEdgeId)) {
          val newVisited = visited + currentEdgeId
          val nextFwd = graph.forwardEdges(currentEdgeId).map(graph(_)).filter(edge => !newVisited(edge.to) && edge.capacity > edge.flow)
          val nextBkd = graph.backwardEdges(currentEdgeId).map(graph(_)).filter(edge => !newVisited(edge.to) && edge.capacity > edge.flow)
          val nextEdges = nextFwd ::: nextBkd
          val (expQ, expP) = expand(tail, prev, nextEdges, currentEdgeId)
          loop(expQ, newVisited, expP)
        } else {
          loop(tail, visited, prev)
        }

      }
    }

    loop(Queue(from))
  }


}
