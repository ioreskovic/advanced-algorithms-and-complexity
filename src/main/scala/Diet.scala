import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import scala.annotation.tailrec
import scala.io.StdIn

object Diet {

  object MatrixRow {
    def apply(s: String): MatrixRow = {
      new MatrixRow(s.split(" ").map(_.toDouble))
    }

    def apply(coeffs: Seq[Double]): MatrixRow = {
      new MatrixRow(coeffs.toArray)
    }
  }

  class MatrixRow private(val coeffs: Array[Double]) {
    def dim: Int = coeffs.length - 1

    def varCoeff(i: Int): Double = coeffs(i)

    def resCoeff: Double = coeffs.last

    def scale(factor: Double): MatrixRow = {
      new MatrixRow(coeffs.map(_ * factor))
    }

    def copy: MatrixRow = {
      MatrixRow(coeffs.clone())
    }

    def scaled(factor: Double): Unit = {
      for (i <- coeffs.indices) {
        coeffs(i) = coeffs(i) * factor
      }
    }

    def neg: MatrixRow = {
      scale(-1.0)
    }

    def negged(): Unit = {
      scaled(-1.0)
    }

    def add(other: MatrixRow): MatrixRow = {
      new MatrixRow((0 to dim).map(i => this.coeffs(i) + other.coeffs(i)).toArray)
    }

    def added(other: MatrixRow, scale: Double = 1.0): Unit = {
      for (i <- this.coeffs.indices) {
        this.coeffs(i) = this.coeffs(i) + (other.coeffs(i) * scale)
      }
    }

    def subtracted(other: MatrixRow, scale: Double = 1.0): Unit = {
      for (i <- this.coeffs.indices) {
        this.coeffs(i) = this.coeffs(i) - (other.coeffs(i) * scale)
      }
    }

    def multiplied(factor: Double): Unit = {
      scaled(factor)
    }

    def divided(factor: Double): Unit = {
      scaled(1.0 / factor)
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
      new MatrixSystem(Array(rows: _*))
    }
  }

  class MatrixSystem private(val m: Array[MatrixRow]) {
    def rows: Int = m.length

    def cols: Int = m.head.dim

    def dim: Int = cols

    def results: Seq[Double] = {
      (0 until dim).map(d => m(d).resCoeff)
    }

    def augmentedRows(indexes: Array[Int], nConstraints: Int, nItems: Int): Array[MatrixRow] = {
      @tailrec
      def loop(i: Int, res: Array[MatrixRow]): Array[MatrixRow] = {
        if (i >= indexes.length) res
        else {
          if (indexes(i) < nConstraints) loop(i + 1, res.updated(i, m(indexes(i)).copy))
          else if (indexes(i) == nConstraints + nItems) loop(i + 1, res.updated(i, MatrixRow(Array.tabulate(nItems + 1)(i => if (i < nItems) 1 else 1E9))))
          else loop(i + 1, res.updated(i, MatrixRow(res(i).copy.coeffs.updated(indexes(i) - nConstraints, -1.0))))
        }
      }

      loop(0, Array.fill(indexes.length)(MatrixRow(Array.fill(nItems + 1)(0.0))))
    }

    def swapRows(i: Int, j: Int): MatrixSystem = {
      val temp = m(i)
      m(i) = m(j)
      m(j) = temp
      this
    }

    def scale(row: Int, factor: Double): MatrixSystem = {
      this (row) = { _: Unit => m(row).scale(factor) }
    }

    def scaled(row: Int, factor: Double): Unit = {
      m(row).scaled(factor)
    }

    def neg(row: Int): MatrixSystem = {
      this (row) = { _: Unit => -m(row) }
    }

    def negged(row: Int): Unit = {
      m(row).negged()
    }

    def add(targetRow: Int, sourceRow: Int): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) + m(sourceRow) }
    }

    def added(targetRow: Int, sourceRow: Int, scale: Double = 1.0): Unit = {
      m(targetRow).added(m(sourceRow), scale)
    }

    def subtract(targetRow: Int, sourceRow: Int): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) - m(sourceRow) }
    }

    def subtracted(targetRow: Int, sourceRow: Int, scale: Double = 1.0): Unit = {
      m(targetRow).subtracted(m(sourceRow), scale)
    }

    def multiply(targetRow: Int, factor: Double): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) * factor }
    }

    def multiplied(targetRow: Int, factor: Double): Unit = {
      m(targetRow).multiplied(factor)
    }

    def divide(targetRow: Int, factor: Double): MatrixSystem = {
      this (targetRow) = { _: Unit => m(targetRow) / factor }
    }

    def divided(targetRow: Int, factor: Double): Unit = {
      m(targetRow).divided(factor)
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

    def partitionByIndex(indexes: Seq[Int], all: Seq[Int]): (MatrixSystem, MatrixSystem) = {
      val posRows = augmentedRows(indexes.toArray, rows, cols)
      val complementedIndexes = all.complement(indexes)
      val negRows = augmentedRows(complementedIndexes.toArray, rows, cols)
      (MatrixSystem(posRows), MatrixSystem(negRows))
    }
  }

  def solve(sys: MatrixSystem): MatrixSystem = {
    for (i <- 0 until sys.dim) {
      step(sys, i)
    }
    scaleToPivots(sys)
    sys
  }

  private def scaleToPivots(sys: MatrixSystem): MatrixSystem = {
    for (i <- 0 until sys.dim) {
//      sys.divide(i, sys.value(i, i))
      sys.divided(i, sys.value(i, i))
    }
    sys
  }

  private def step(sys: MatrixSystem, column: Int): Unit = {
    val (pivotRow, pivotCol) = findPivot(sys, column)
    sys.swapRows(column, pivotRow)

    for (row <- 0 until sys.dim) {
      if (row != column) {
        val factor = sys.value(row, column) / sys.value(column, column)

        // IMMUTABLE
//        sys(row) = { _ => sys(row) - (sys(column) * factor) }

        // MUTABLE
        sys.subtracted(row, column, factor)
      }
    }
  }

  def findPivot(sys: MatrixSystem, col: Int): (Int, Int) = {
    ((col until sys.dim).maxBy(row => math.abs(sys.value(row, col))), col)
  }

  def main(args: Array[String]): Unit = {
    val metaInfo = StdIn.readLine().split(" ")
    val (nInequalities, nItems) = (metaInfo(0).toInt, metaInfo(1).toInt)
    val lhss = (0 until nInequalities).map(_ => StdIn.readLine())
    val rhss = StdIn.readLine().split(" ")
    val functionCoeffs = StdIn.readLine.split(" ").map(_.toDouble).toVector
    val inputRows = lhss.zip(rhss).map { case (lhs, rhs) => MatrixRow(lhs + " " + rhs) }
    val inputSystem = MatrixSystem(inputRows)

    val all = 0 to (nInequalities + nItems)

    var comboResults: List[(Double, Seq[Double])] = Nil
    for (c <- all.choose(nItems)) {
      val (posSys, negSys) = inputSystem.partitionByIndex(c, all)
      val results = solve(posSys).results
      if (isSolution(negSys, results)) {
        comboResults = (functionValue(functionCoeffs, results), results) :: comboResults
      }
    }

    if (comboResults.isEmpty) {
      println("No solution")
    } else {
      val (maxValue, maxArgs) = comboResults.maxBy(_._1)
      if (maxValue >= 1E9) {
        println("Infinity")
      } else {
        println("Bounded solution")
        println(maxArgs.mkString(" "))
      }
    }

//    val comboResults = for {
//      c <- all.choose(nItems)
//      (posSys, negSys) = inputSystem.partitionByIndex(c, all)
//      results = solve(posSys).results
//      if isSolution(negSys, results)
//    } yield (functionValue(functionCoeffs, results), results)
//
//    comboResults match {
//      case Nil => println("No solution")
//      case cs @ _ => {
//        val (maxValue, maxArgs) = cs.maxBy(_._1)
//        if (maxValue >= 1E9) {
//          println("Infinity")
//        } else {
//          println("Bounded solution")
//          println(maxArgs.mkString(" "))
//        }
//      }
//    }
  }

  def isSolution(system: MatrixSystem, doubles: Seq[Double]): Boolean = {
    system.m.forall(row => functionValue(row.coeffs.init, doubles) <= row.resCoeff)
  }

  implicit class Combinations[T](from: Seq[T]) {
    def choose(n: Int): Seq[Seq[T]] = if (n > from.length) Seq() else from.combinations(n).toSeq
    def complement(combination: Seq[T]): Seq[T] = {
      val used = combination.toSet
      from.filterNot(used)
    }
  }

  def functionValue(coeffs: Seq[Double], vars: Seq[Double]): Double = {
//    coeffs.zip(vars).map { case (c, v) => c * v }.sum
    var sum = 0.0
    for (i <- coeffs.indices) {
      sum = sum + (coeffs(i) * vars(i))
    }
    sum
  }

}
