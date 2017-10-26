package week4.wip

trait CNFClause {
}

object CNFClause {
  case class CNF0() extends CNFClause
  case class CNF1(t0: CNFTerm) extends CNFClause
  case class CNF2(t0: CNFTerm, t1: CNFTerm) extends CNFClause

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

