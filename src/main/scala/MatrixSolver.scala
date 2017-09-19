import EnergyValues.{MatrixRow, MatrixSystem, solve}

import scala.io.StdIn

object MatrixSolver {
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
//    println(s"Solving for pivot at column $column")
      val (pivotRow, pivotCol) = findPivot(sys, column)
//    println(s"Current pivot position before swap: ($pivotRow, $pivotCol)")
//    println(sys)
//    println()
      sys.swapRows(column, pivotRow)
//    println(s"Swapped pivot")
//    println(sys)
//    println()
//    println("Reducing other rows")
      (0 until sys.dim).foreach(row => if (row != column) {
//        println(s"Reducing row $row")
        val factor = sys.value(row, column) / sys.value(column, column)
//        println(s"Scaling pivot row by factor $factor")
//        println(s"Nulling current row pivot position")
        sys(row) = { _ => sys(row) - (sys(column) * factor) }
//        println(sys)
//        println()
      })
//    println(s"Step $column is done")
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
      val solution = (0 until sys.dim).map(d => sys(d).resCoeff)
      println(solution.mkString(" "))
    } else {
      println()
    }
  }
}
