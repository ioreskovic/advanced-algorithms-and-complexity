package week4.wip

case class StronglyConnectedComponent[Vertex](id: Int, vertices: Set[Vertex]) {
  def vertexMappings: Map[Vertex, Int] = {
    vertices.foldLeft(Map[Vertex, Int]()){ case (m, v) => m + (v -> id)}
  }
}

object StronglyConnectedComponent {
}
