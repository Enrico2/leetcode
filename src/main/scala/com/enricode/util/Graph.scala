package com.enricode.util

import scala.collection.mutable

class Graph[T](in: Array[Array[T]], directed: Boolean) {
  val vertices: Map[T, Node[T]] = {
    val m = mutable.Map[T, Node[T]]()

    in.foreach { case Array(a, b) =>
      val aNode = m.getOrElse(a, Node(a))
      val bNode = m.getOrElse(b, Node(b))
      val edge = Edge(aNode, bNode)

      m(a) = aNode.copy(edges = aNode.edges :+ edge)
      if (!directed)
        m(b) = bNode.copy(edges = bNode.edges :+ edge)
    }

    m.toMap
  }

  override def toString: String = {
    vertices.map { case (data, node) =>
      s"$data -> " + node.edges.map { edge => s"(${edge.n1.data}, ${edge.n2.data})" }.mkString("[", ",", "]")
    }.mkString("\n")
  }

}
