import scala.collection.mutable.ListBuffer

object LinearSystem {
  def apply(): LinearSystem = empty()

  def empty(): LinearSystem = {
    new LinearSystem(ListBuffer())
  }

  def apply(linearInequality: LinearInequality*): LinearSystem = {
    new LinearSystem(ListBuffer(linearInequality:_*))
  }
}

class LinearSystem private(private val m: ListBuffer[LinearInequality]) {
  def swapRows(i: Int, j: Int): LinearSystem = {
    val temp = m(i)
    m(i) = m(j)
    m(j) = temp
    this
  }

  def scale(row: Int, factor: Double): LinearSystem = {
    this(row) = { _: Unit => m(row).scale(factor) }
  }

  def neg(row: Int): LinearSystem = {
    this(row) = { _: Unit => -m(row) }
  }

  def add(targetRow: Int, sourceRow: Int): LinearSystem = {
    this(targetRow) = { _: Unit => m(targetRow) + m(sourceRow) }
  }

  def subtract(targetRow: Int, sourceRow: Int): LinearSystem = {
    this(targetRow) = { _: Unit => m(targetRow) - m(sourceRow) }
  }

  def multiply(targetRow: Int, factor: Double): LinearSystem = {
    this(targetRow) = { _: Unit => m(targetRow) * factor }
  }

  def divide(targetRow: Int, factor: Double): LinearSystem = {
    this(targetRow) = { _: Unit => m(targetRow) / factor }
  }

  def update(row: Int, f: Unit => LinearInequality): LinearSystem = {
    m(row) = f()
    this
  }

  def apply(row: Int): LinearInequality = m(row)

  override def toString: String = {
    m.mkString("\n")
  }
}
