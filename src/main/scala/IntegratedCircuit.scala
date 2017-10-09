import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object IntegratedCircuit {
  trait CNFTerm {
    def variable: Int
    def sign: Boolean
    def index: Int
    def unary_-(): CNFTerm
  }

  case class PositiveCNFTerm(variable: Int) extends CNFTerm {
    override val sign: Boolean = true
    override val index: Int = (variable - 1) << 1
    override def unary_-(): NegativeCNFTerm = NegativeCNFTerm(variable)
    override lazy val toString: String = s"$variable"
  }

  case class NegativeCNFTerm(variable: Int) extends CNFTerm {
    override val sign: Boolean = false
    override val index: Int = (variable << 1) - 1
    override def unary_-(): PositiveCNFTerm = PositiveCNFTerm(variable)
    override lazy val toString: String = s"-$variable"
  }

  object CNFTerm {
    def apply(variable: Int): CNFTerm = if (variable > 0) {
      PositiveCNFTerm(variable)
    } else if (variable < 0) {
      NegativeCNFTerm(-variable)
    } else {
      throw new IllegalArgumentException(s"Invalid variable number: $variable")
    }
  }

  trait CNFClause {
  }

  case class CNF0() extends CNFClause
  case class CNF1(t0: CNFTerm) extends CNFClause
  case class CNF2(t0: CNFTerm, t1: CNFTerm) extends CNFClause

  object CNFClause {
    def apply(s: String): CNFClause = {
      if (s.isEmpty) CNF0()
      else {
        val info = s.split(" ")
        if (info.length == 1) CNF1(CNFTerm(info(0).toInt))
        else if (info.length == 2) CNF2(CNFTerm(info(0).toInt), CNFTerm(info(1).toInt))
        else throw new IllegalArgumentException(s"Invalid input: $s")
      }
    }
  }

  abstract class Graph[Vertex](implicit conversion: Vertex => Int) {
    def transpose: Graph[Vertex]
    def apply(vertex: Vertex): List[Vertex] = adj(vertex)
    def adj: Vertex => List[Vertex]
    def postOrder: List[Vertex] = {
      import scala.collection.mutable.{HashSet => MutableHashSet}
      val order = ListBuffer[Vertex]()
      val visited = MutableHashSet[Int]()

      // recursive needs to be iterative
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
      import scala.collection.mutable.{HashMap => MutableHashMap, HashSet => MutableHashSet, Queue => MutableQueue}
      val reversePostOrder = postOrder.reverse
      val sccMap = MutableHashMap[Int, List[Vertex]]().withDefaultValue(Nil)
      val t = transpose
      val visited = MutableHashSet[Int]()

      // recursive to iterative

      def exploreIterative(vertex: Vertex, sccIndex: Int): Unit = {
        var stack = List[Vertex](vertex)

        while (stack.nonEmpty) {
          val v = stack.head
          stack = stack.tail
          visited(v) = true
          sccMap.update(sccIndex, v :: sccMap(sccIndex))
          t(v).foreach(n => if (!visited(n)) stack = n :: stack)
        }
      }

//      def explore(vertex: Vertex, sccIndex: Int): Unit = {
//        visited(vertex) = true
//        sccMap.update(sccIndex, vertex :: sccMap(sccIndex))
//        t(vertex).foreach(n => if (!visited(n)) explore(n, sccIndex))
//      }

      @tailrec
      def loop(vx: List[Vertex], sccIndex: Int = 0): Unit = vx match {
        case v :: vs if !visited(v) => {
          exploreIterative(v, sccIndex)
          loop(vs, sccIndex + 1)
        }
        case _ :: vs => loop(vs, sccIndex)
        case Nil =>
      }

      loop(reversePostOrder)
      sccMap.map{ case (id, vertices) => StronglyConnectedComponent(id, vertices.toSet)}.toList
    }

    def nVertices: Int = vertices.length
    def vertices: List[Vertex]
  }

  case class Edge[Vertex](from: Vertex, to: Vertex)

  case class StronglyConnectedComponent[Vertex](id: Int, vertices: Set[Vertex]) {
    def vertexMappings: Map[Vertex, Int] = {
      vertices.foldLeft(Map[Vertex, Int]()){ case (m, v) => m + (v -> id)}
    }
  }

  object StronglyConnectedComponent {
  }

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

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val nVariables = metaInfo(0).toInt
    val vertices = (1 to nVariables).foldLeft(List[CNFTerm]()){ case (vs, i) => PositiveCNFTerm(i) :: NegativeCNFTerm(i) :: vs}
    val nClauses = metaInfo(1).toInt
    val clauses = (0 until nClauses).map(_ => CNFClause(StdIn.readLine()))
    val graphEdges = clauses.flatMap(clauseToEdges)
    val cnf2graph = DirectedGraph(vertices, graphEdges)(_.index)
    val scc = cnf2graph.scc
    if (!isSatisfiable(scc)) {
      println("UNSATISFIABLE")
    } else {
      val sccGraph = sccToGraph(cnf2graph, scc)
      val topoSort = sccGraph.postOrder
      val sat = assignment(topoSort).filter(_._2).keys.toList.sortBy(t => math.abs(t.index))
      println("SATISFIABLE")
      println(sat.mkString(" "))
    }
  }

  def isSatisfiable(sccList: List[StronglyConnectedComponent[CNFTerm]]): Boolean = {
    for (scc <- sccList) {
      for (term <- scc.vertices) {
        if (scc.vertices(-term)) {
          return false
        }
      }
    }

    true
  }

  def assignment(sccList: List[StronglyConnectedComponent[CNFTerm]]): Map[CNFTerm, Boolean] = {
    import scala.collection.mutable.{ HashMap => MutableHashMap }

//    println("TOPO-SCCS")
//    println(sccList.mkString("\n"))

    val assignments = MutableHashMap[CNFTerm, Boolean]()

    for (scc <- sccList) {
      for (term <- scc.vertices) {
        if (!assignments.contains(term)) {
          assignments(term) = true
          assignments(-term) = false
        }
      }
    }

    assignments.toMap
  }

  def clauseToEdges(c: CNFClause): List[Edge[CNFTerm]] = c match {
    case CNF2(a, b) => List(Edge(-a, b), Edge(-b, a))
    case CNF1(a) => List(Edge(-a, a))
    case _ => Nil
  }

  def sccToGraph(graph: Graph[CNFTerm], scc: List[StronglyConnectedComponent[CNFTerm]]): Graph[StronglyConnectedComponent[CNFTerm]] = {
    import scala.collection.mutable.{HashSet => MutableHashSet}
    val sccMap: Map[Int, StronglyConnectedComponent[CNFTerm]] = scc.foldLeft(Map[Int, StronglyConnectedComponent[CNFTerm]]()){ case (m, s) => m + (s.id -> s) }
    val vertexSccs = scc.foldLeft(Map[CNFTerm, Int]()){ case (m, s) => m ++ s.vertexMappings }
    val sccMappings = MutableHashSet[(Int, Int)]()

    for (from <- graph.vertices) {
      for (to <- graph(from)) {
        val fromScc = vertexSccs(from)
        val toScc = vertexSccs(to)
        if (fromScc != toScc) {
          sccMappings((fromScc, toScc)) = true
        }
      }
    }

//    val sccMappings = (for {
//      from <- graph.vertices
//      to <- graph(from)
//      if vertexSccs(from) != vertexSccs(to)
//    } yield (vertexSccs(from), vertexSccs(to))).toSet

    val edges = sccMappings.map{ case (fromId, toId) => Edge(sccMap(fromId), sccMap(toId))}
    DirectedGraph(scc, edges.toSeq)(_.id)
  }

}
