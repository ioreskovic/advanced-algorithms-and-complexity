object MatrixSystem {
  def apply(rows: Seq[MatrixRow]): MatrixSystem = {
    new MatrixSystem(Array(rows:_*))
  }
}

class MatrixSystem private(private val m: Array[MatrixRow]) {
  def rows: Int = m.length
  def cols: Int = m.head.dim
  def dim: Int = cols

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
