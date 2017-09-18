object LinearInequality {
  def apply(s: String): LinearInequality = {
    new LinearInequality(s.split(" ").map(_.toDouble))
  }
}

class LinearInequality private(private val coeffs: Array[Double]) {
  def dim: Int = coeffs.length - 1
  def varCoeff(i: Int): Double = coeffs(i)
  def resCoeff: Double = coeffs.last

  def scale(factor: Double): LinearInequality = {
    new LinearInequality(coeffs.map(_ * factor))
  }

  def neg: LinearInequality = {
    scale(-1.0)
  }

  def add(other: LinearInequality): LinearInequality = {
    new LinearInequality((0 to dim).map(i => this.coeffs(i) + other.coeffs(i)).toArray)
  }

  def +(other: LinearInequality): LinearInequality = add(other)
  def unary_-(): LinearInequality = neg
  def -(other: LinearInequality): LinearInequality = this + (-other)
  def *(factor: Double): LinearInequality = scale(factor)
  def /(factor: Double): LinearInequality = scale(1.0 / factor)

  override def toString: String = {
    (0 until dim).map(d => varCoeff(d).formatted("%+f") + s"x[$d]").mkString(" ") + " = " + resCoeff.formatted("%+f")
  }
}

