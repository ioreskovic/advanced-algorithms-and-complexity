package week4.wip

trait CNFTerm {
  def variable: Int
  def sign: Boolean
  def index: Int
  def unary_-(): CNFTerm
}

object CNFTerm {
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

  def apply(variable: Int): CNFTerm = if (variable > 0) {
    PositiveCNFTerm(variable)
  } else if (variable < 0) {
    NegativeCNFTerm(-variable)
  } else {
    throw new IllegalArgumentException(s"Invalid variable number: $variable")
  }
}
