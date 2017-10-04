object CNF2Graph {
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
    val adjMap = SAT2(lines.map(s => CNF(s))).edgePairs.flatMap(ep => Seq(ep.e0, ep.e1)).foldLeft(Map[Int, List[Int]]().withDefaultValue(List())) { case (adj, e) => adj + (e.fromIndex -> (e.toIndex :: adj(e.fromIndex))) }
    CNF2Graph(nVariables * 2, adjMap)
  }
}

case class CNF2Graph(nVertices: Int, adj: Map[Int, List[Int]]) {
  
}
