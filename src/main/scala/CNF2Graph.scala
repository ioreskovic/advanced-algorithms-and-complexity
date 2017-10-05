import CNF2Graph.{SCC, Vertex}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object CNF2Graph {
  type Vertex = Int
  type SCC = Int

  trait Term {
    def variable: Int
    def sign: Boolean
    def index: Int
    def unary_-(): Term
  }

  object Term {
    def apply(variable: Int): Term = if (variable > 0) {
      PositiveTerm(variable)
    } else if (variable < 0) {
      NegativeTerm(-variable)
    } else {
      throw new IllegalArgumentException(s"Invalid variable number: $variable")
    }
  }

  case class PositiveTerm(variable: Int) extends Term {
    override val sign: Boolean = true
    override val index: Int = (variable - 1) << 1
    override def unary_-(): NegativeTerm = NegativeTerm(variable)
  }

  case class NegativeTerm(variable: Int) extends Term {
    override val sign: Boolean = false
    override val index: Int = (variable << 1) - 1
    override def unary_-(): PositiveTerm = PositiveTerm(variable)
  }

  trait CNF {
  }

  object CNF {
    def apply(s: String): CNF = {
      if (s.isEmpty) CNF0()
      else {
        val info = s.split(" ")
        if (info.length == 1) CNF1(Term(info(0).toInt))
        else if (info.length == 2) CNF2(Term(info(0).toInt), Term(info(1).toInt))
        else throw new IllegalArgumentException(s"Invalid input: $s")
      }
    }
  }

  case class CNF0() extends CNF
  case class CNF1(t0: Term) extends CNF
  case class CNF2(t0: Term, t1: Term) extends CNF

  case class SAT2(cnfs: Seq[CNF]) {
    def edgePairs: Seq[CNF2EdgePair] = cnfs.collect{ case cnfN => cnfN match {
      case CNF2(t0, t1) => CNF2EdgePair(CNF2Edge(-t0.index, t1.index), CNF2Edge(-t1.index, t0.index))
    }}
  }

  case class CNF2Edge(fromIndex: Int, toIndex: Int)
  case class CNF2EdgePair(e0: CNF2Edge, e1: CNF2Edge)

  def apply(nVariables: Int)(lines: Seq[String]): CNF2Graph = {
    val adjMap = SAT2(lines.map(s => CNF(s))).edgePairs.flatMap(ep => Seq(ep.e0, ep.e1)).foldLeft(Map[Vertex, List[Vertex]]().withDefaultValue(Nil)) { case (adj, e) => adj + (e.fromIndex -> (e.toIndex :: adj(e.fromIndex))) }
    CNF2Graph(nVariables * 2, adjMap)
  }
}

case class CNF2Graph(nVertices: Int, adj: Map[Vertex, List[Vertex]]) {
  def transpose: CNF2Graph = {
    val newAdj = adj.foldLeft(Map[Vertex, List[Vertex]]().withDefaultValue(Nil)){
      case (adjT, (from, tos)) => tos.foldLeft(adjT){
        case (adjTi, to) => adjTi + (to -> (from :: adjTi(to)))
      }
    }

    CNF2Graph(nVertices, newAdj)
  }

  def neighbours(vertex: Vertex): List[Vertex] = adj(vertex)
  def apply(vertex: Vertex): List[Vertex] = neighbours(vertex)
  def hasEdge(from: Vertex, to: Vertex): Boolean = adj(from).contains(to)

  def scc: Map[SCC, List[Vertex]] = {
    import scala.collection.mutable.{HashMap => MutableHashMap, HashSet => MutableHashSet}
    val reversePostOrder = postOrder.reverse
    val sccMap = MutableHashMap[SCC, List[Vertex]]().withDefaultValue(Nil)
    val gT = transpose
    val visited = MutableHashSet[Vertex]()

    def explore(vertex: Vertex, scc: SCC): Unit = {
      visited(vertex) = true
      sccMap.update(scc, vertex :: sccMap(scc))
      gT(vertex).foreach(n => if (!visited(n)) explore(n, scc))
    }

    @tailrec
    def loop(vx: List[Vertex], scc: SCC = 0): Unit = vx match {
      case v :: vs if !visited(v) => {
        explore(v, scc)
        loop(vs, scc + 1)
      }
      case _ :: vs => loop(vs, scc)
      case Nil =>
    }

    loop(reversePostOrder)
    sccMap.toMap
  }

  def postOrder: List[Vertex] = {
    import scala.collection.mutable.{HashSet => MutableHashSet}
    val order = ListBuffer[Vertex]()
    val visited = MutableHashSet[Vertex]()

    def dfsLoop(vertex: Vertex): Unit = {
      if (!visited(vertex)) {
        visited(vertex) = true
        adj(vertex).foreach(neighbour => dfsLoop(neighbour))
        order.append(vertex)
      }
    }

    for (vertex <- 0 until nVertices) {
      dfsLoop(vertex)
    }

    order.toList
  }

}
