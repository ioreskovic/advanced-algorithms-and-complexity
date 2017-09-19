import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import scala.io.StdIn

object Diet {
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

  object MatrixSystem {
    def apply(rows: Seq[MatrixRow]): MatrixSystem = {
      new MatrixSystem(Array(rows:_*))
    }
  }

  class MatrixSystem private(private val m: Array[MatrixRow]) {
    def rows: Int = m.length
    def cols: Int = m.head.dim
    def dim: Int = math.min(rows, cols)

    def swapRows(i: Int, j: Int): MatrixSystem = {
      val temp = m(i)
      m(i) = m(j)
      m(j) = temp
      this
    }

    def scale(row: Int, factor: Double): MatrixSystem = {
      this (row) = { _: Unit => m(row).scale(factor) }
    }

    def neg(row: Int): MatrixSystem = {
      this (row) = { _: Unit => -m(row) }
    }

    def add(targetRow: Int, sourceRow: Int): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) + m(sourceRow) }
    }

    def subtract(targetRow: Int, sourceRow: Int): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) - m(sourceRow) }
    }

    def multiply(targetRow: Int, factor: Double): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) * factor }
    }

    def divide(targetRow: Int, factor: Double): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) / factor }
    }

    def update(row: Int, f: Unit => MatrixRow): MatrixSystem = {
      m(row) = f()
      this
    }

    def apply(row: Int): MatrixRow = m(row)

    def value(row: Int, column: Int): Double = m(row).varCoeff(column)

    override def toString: String = {
      m.mkString(System.lineSeparator())
    }
  }

  def solve(sys: MatrixSystem): MatrixSystem = {
    (0 until sys.dim).foreach(step(sys, _))
    scaleToPivots(sys)
    sys
  }

  private def scaleToPivots(sys: MatrixSystem): MatrixSystem = {
    (0 until sys.dim).foreach(i => sys.divide(i, sys.value(i, i)))
    sys
  }

  private def step(sys: MatrixSystem, column: Int) = {
    val (pivotRow, pivotCol) = findPivot(sys, column)
    sys.swapRows(column, pivotRow)
    (0 until sys.dim).foreach(row => if (row != column) {
      val factor = sys.value(row, column) / sys.value(column, column)
      sys(row) = { _ => sys(row) - (sys(column) * factor) }
    })
  }

  def findPivot(sys: MatrixSystem, col: Int): (Int, Int) = {
    ((col until sys.dim).maxBy(row => math.abs(sys.value(row, col))), col)
  }

  def main(args: Array[String]): Unit = {
    val dim = StdIn.readLine().toInt
    if (dim > 0) {
      val rows = (0 until dim).map(_ => MatrixRow(StdIn.readLine()))
      val sys = MatrixSystem(rows)
      solve(sys)
      println(sys)
      println()
      val solution = (0 until sys.dim).map(d => sys(d).resCoeff)
      println(solution.mkString(" "))
    } else {
      println()
    }
  }

  implicit class Combinations[T](from: Seq[T]) {
    def choose(n: Int): Iterator[Seq[T]] = from.combinations(n)
  }
}
