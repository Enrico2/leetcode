package com.enricode.util

class ConnectedGraph[T](in: Array[Array[T]]) {
  private[this] val m = in.foldLeft[Map[T, Node[T]]](Map.empty) { case (map, Array(a, b)) =>
    val aNode = map.getOrElse(a, Node(a))
    val bNode = map.getOrElse(b, Node(b))
    val edge = Edge(aNode, bNode)

    map ++ Map(
      aNode.data -> aNode.copy(edges = aNode.edges :+ edge),
      bNode.data -> bNode.copy(edges = bNode.edges :+ edge)
    )
  }

  override def toString: String = {
    m.map { case (data, node) =>
      s"$data -> " + node.edges.map { edge => s"(${edge.n1.data}, ${edge.n2.data})" }.mkString("[", ",", "]")
    }.mkString("\n")
  }

  def dfs(visit: Node[T] => Unit)(alreadyVisited: Node[T] => Unit): Unit = {
    GraphSearch.dfs[Node[T]](
      m.head._2,
      _.edges.toSet.map { e: Edge[T] => (e.n1, e.n2) },
      visit,
      alreadyVisited
    )
  }

  def bfs(visit: Node[T] => Unit)(alreadyVisited: Node[T] => Unit): Unit = {
    GraphSearch.bfs[Node[T]](
      m.head._2,
      _.edges.toSet.map { e: Edge[T] => (e.n1, e.n2) },
      visit,
      alreadyVisited
    )
  }
}




