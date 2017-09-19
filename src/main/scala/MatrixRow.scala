import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

object MatrixRow {
  def apply(s: String): MatrixRow = {
    new MatrixRow(s.split(" ").map(_.toDouble))
  }
}

class MatrixRow private(private val coeffs: Array[Double]) {
  def dim: Int = coeffs.length - 1
  def varCoeff(i: Int): Double = coeffs(i)
  def resCoeff: Double = coeffs.last

  def scale(factor: Double): MatrixRow = {
    new MatrixRow(coeffs.map(_ * factor))
  }

  def neg: MatrixRow = {
    scale(-1.0)
  }

  def add(other: MatrixRow): MatrixRow = {
    new MatrixRow((0 to dim).map(i => this.coeffs(i) + other.coeffs(i)).toArray)
  }

  def +(other: MatrixRow): MatrixRow = add(other)
  def unary_-(): MatrixRow = neg
  def -(other: MatrixRow): MatrixRow = this + (-other)
  def *(factor: Double): MatrixRow = scale(factor)
  def /(factor: Double): MatrixRow = scale(1.0 / factor)

  override def toString: String = {
    val df = new DecimalFormat("+00,000.00000;-00,000.00000", new DecimalFormatSymbols(Locale.ENGLISH))
    (0 until dim).map(d => df.format(varCoeff(d)) + s"x[$d]").mkString(" ") + " = " + df.format(resCoeff)
  }
}

