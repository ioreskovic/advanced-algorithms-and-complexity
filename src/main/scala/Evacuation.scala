import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.StdIn

object Evacuation {
  import FlowGraph.{BackwardEdgeIds, ForwardEdgeIds}

  object FlowGraph {
    type ForwardEdgeId = Int
    type ForwardEdgeIds = List[ForwardEdgeId]

    type BackwardEdgeId = Int
    type BackwardEdgeIds = List[BackwardEdgeId]

    def empty: FlowGraph = new FlowGraph(Map().withDefaultValue((List(), List())), Map())
    def apply(edges : FlowEdge*): FlowGraph = edges.foldLeft(empty)(_ + _)
  }

  class FlowGraph(adj: Map[Int, (ForwardEdgeIds, BackwardEdgeIds)], edges: Map[Int, FlowEdge]) {
    def apply(edgeId: Int): FlowEdge = edges(edgeId)

    def edgeIdFor(fromVertex: Int, toVertex: Int): Option[Int] = {
      val (fwEs, bkEs) = adj(fromVertex)
      (fwEs ::: bkEs).find(eId => edges(eId).to == toVertex)
    }

    def edgeFor(fromVertex: Int, toVertex: Int): Option[(Int, Int)] = {
      val (fwEs, bkEs) = adj(fromVertex)
      (fwEs ::: bkEs).find(eId => edges(eId).to == toVertex) match {
        case None => None
        case Some(id) => Some(id, edges(id).capacity - edges(id).flow)
      }
    }

    def +(e: FlowEdge): FlowGraph = {
      val b = e.reversed
      val (real, bwd) = adj(e.from)
      val (fwd, imaginary) = adj(b.from)
      val fwdEdgeId = edges.size
      val bwdEdgeId = fwdEdgeId + 1
      new FlowGraph(adj + (e.from -> (fwdEdgeId :: real, bwd), b.from -> (fwd, bwdEdgeId :: imaginary)), edges + (fwdEdgeId -> e, bwdEdgeId -> b))
    }

    def size: Int = adj.size

    def forwardEdges(id: Int): ForwardEdgeIds = adj(id)._1

    def backwardEdges(id: Int): BackwardEdgeIds = adj(id)._2

    private def flowAugmentedEdges(edgeId: Int, flow: Int): List[(Int, FlowEdge)] = {
      val fwId = edgeId
      val bkId = edgeId ^ 1
      val fwEdge = edges(fwId)
      val bkEdge = edges(bkId)
      val newFwEdge = fwEdge + flow
      val newBkEdge = bkEdge - flow
      List(fwId -> newFwEdge, bkId -> newBkEdge)
    }

    def withFlow(edgeId: Int, flow: Int): FlowGraph = {
      new FlowGraph(adj, flowAugmentedEdges(edgeId, flow).foldLeft(edges){ case (es, e) => es + e})
    }

    def withFlow(edgeIds: List[Int], flow: Int): FlowGraph = {
      new FlowGraph(adj, edgeIds.flatMap(id => flowAugmentedEdges(id, flow)).foldLeft(edges){ case (es, e) => es + e })
    }

    override lazy val toString: String = {
      s"FlowGraph[\n" +
        s"\tEdges: ${edges.toList.sortBy(_._1).mkString("\n\t\t", "\n\t\t", "")}\n" +
        s"\tAdj: ${adj.toList.sortBy(_._1).mkString("\n\t\t", "\n\t\t", "")}\n" +
        s"]"
    }
  }

  case class FlowEdge(from: Int, to: Int, capacity: Int, flow: Int = 0) {
    def reversed: FlowEdge = FlowEdge(to, from, 0)
    def +(moreFlow: Int): FlowEdge = FlowEdge(from, to, capacity, flow + moreFlow)
    def -(lessFlow: Int): FlowEdge = FlowEdge(from, to, capacity, flow - lessFlow)
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nVertices = metaInfo(0).toInt
    val nEdges = metaInfo(1).toInt
    val edges = compress((0 until nEdges).map(i => parseEdge(StdIn.readLine())).toList)
    val graph = FlowGraph(edges: _*)
    val flow = maxflow(graph, 0, nVertices - 1)
    println(flow)
  }

  def parseEdge(str: String): FlowEdge = {
    val info = str.split(" ")
    FlowEdge(info(0).toInt - 1, info(1).toInt - 1, info(2).toInt)
  }

  def compress(edges: List[FlowEdge]): List[FlowEdge] = {
    edges.filterNot(e => e.from == e.to).groupBy(e => (e.from, e.to)).mapValues(_.map(_.capacity).sum).toList.map{ case ((from, to), cap) => FlowEdge(from, to, cap) }
  }

  def maxflow(graph: FlowGraph, source: Int, sink: Int): Int = {
    @tailrec
    def loop(g: FlowGraph, totalFlow: Int = 0): Int = {
      val path = augmentingPath(g, source, sink)
      val edgeIds = path.sliding(2).map(lx => (lx(0), lx(1))).flatMap { case (from, to) => g.edgeFor(from, to) }.toList
      if (edgeIds.nonEmpty) {
        val f = math.min(Integer.MAX_VALUE, edgeIds.minBy(_._2)._2)
        if (f > 0 && f != Integer.MAX_VALUE) {
          loop(g.withFlow(edgeIds.unzip._1, f), totalFlow + f)
        } else {
          totalFlow
        }
      } else {
        totalFlow
      }
    }

    loop(graph)
  }

  def augmentingPath(graph: FlowGraph, from: Int, to: Int): List[Int] = {
    @tailrec
    def expand(q: Queue[Int], prev: Map[Int, Int], edges: List[FlowEdge], curr: Int): (Queue[Int], Map[Int, Int]) = edges match {
      case e :: es => {
        val newQ = q.enqueue(e.to)
        val newP = if (!prev.contains(e.to)) prev + (e.to -> curr) else prev
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
          val nextFwd = graph.forwardEdges(currentEdgeId).map(graph(_)).filter(edge => !newVisited(edge.to) && edge.capacity - edge.flow > 0)
          val nextBkd = graph.backwardEdges(currentEdgeId).map(graph(_)).filter(edge => !newVisited(edge.to) && edge.capacity - edge.flow > 0)
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
