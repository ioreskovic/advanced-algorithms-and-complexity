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

  def +(e: FlowEdge): FlowGraph = {
    val b = e.reversed
    val (fwd, bwd) = adj(e.from)
    val fwdEdgeId = edges.size
    val bwdEdgeId = fwdEdgeId + 1
    new FlowGraph(adj + (e.from -> (fwdEdgeId :: fwd, bwdEdgeId :: bwd)), edges + (fwdEdgeId -> e, bwdEdgeId -> b))
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
