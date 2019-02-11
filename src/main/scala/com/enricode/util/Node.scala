package com.enricode.util

case class Node[T](data: T, edges: List[Edge[T]] = Nil)
