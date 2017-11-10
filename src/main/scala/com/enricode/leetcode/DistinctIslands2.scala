package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://leetcode.com/problems/number-of-distinct-islands-ii
  */
object DistinctIslands2 extends LeetcodeApp {
  import com.enricode.util.Util.inBounds

  override def run(): Unit = {
    val sea1 =
      Array(
        Array(1, 1, 0, 0, 0),
        Array(1, 0, 1, 0, 0),
        Array(0, 0, 1, 0, 1),
        Array(0, 0, 0, 1, 1))

    val sea2 =
      Array(
        Array(1,0,1,1,1,0,1,0,1,0,0,0,1,1,0),
        Array(1,1,1,0,1,0,1,1,0,1,0,1,1,0,1),
        Array(1,0,1,1,0,0,0,1,0,1,0,0,0,0,0),
        Array(1,1,1,0,0,1,0,0,0,1,0,1,0,0,0),
        Array(0,0,0,1,1,0,1,0,0,0,1,0,1,0,1),
        Array(0,1,0,1,0,0,0,1,0,0,1,1,1,0,1),
        Array(1,1,1,0,0,0,0,0,0,0,0,0,1,0,1),
        Array(0,1,1,1,1,0,0,1,1,1,0,1,0,1,1),
        Array(1,1,1,1,0,0,1,0,1,0,1,1,1,0,0),
        Array(1,0,1,1,1,1,1,1,1,0,1,1,1,0,1))

    // x,y,z,a,b,c,d,e,f
//    val sea3 =
//      Array(
        //    0         5         10      14
        // Array(X,0,X,X,X,0,Y,0,A,0,0,0,Y,Y,0), // 0
        // Array(X,X,X,0,X,0,Y,Y,0,Z,0,Y,Y,0,A), // 1
        // Array(X,0,X,X,0,0,0,Y,0,Z,0,0,0,0,0), // 2
        // Array(X,X,X,0,0,A,0,0,0,Z,0,A,0,0,0), // 3
        // Array(0,0,0,E,E,0,A,0,0,0,D,0,D,0,B), // 4
        // Array(0,F,0,E,0,0,0,A,0,0,D,D,D,0,B), // 5
        // Array(F,F,F,0,0,0,0,0,0,0,0,0,D,0,B), // 6
        // Array(0,F,F,F,F,0,0,F,F,F,0,C,0,B,B), // 7
        // Array(F,F,F,F,0,0,F,0,F,0,C,C,C,0,0), // 8
        // Array(F,0,F,F,F,F,F,F,F,0,C,C,C,0,A)) // 9

    /*
    Z => 31164468 -> ListBuffer(Island(List((1,9), (2,9), (3,9))))
    E => 1891858814 -> ListBuffer(Island(List((4,3), (5,3), (4,4))))
    B => -536293062 -> ListBuffer(Island(List((4,14), (5,14), (6,14), (7,14), (7,13))))
    A => -1161629283 -> ListBuffer(Island(List((0,8))), Island(List((1,14))), Island(List((3,5))), Island(List((3,11))), Island(List((4,6))), Island(List((5,7))), Island(List((9,14))))
    Y => 178854735 -> ListBuffer(Island(List((0,6), (1,6), (1,7), (2,7))))
    Y2 -879265241 -> ListBuffer(Island(List((0,12), (1,12), (0,13), (1,11))))
    X => -1307662615 -> ListBuffer(Island(List((0,0), (1,0), (2,0), (1,1), (3,0), (1,2), (3,1), (0,2), (2,2), (3,2), (0,3), (2,3), (0,4), (1,4))))
    D => 1150053298 -> ListBuffer(Island(List((4,10), (5,10), (5,11), (5,12), (4,12), (6,12))))
    C => 685868175 -> ListBuffer(Island(List((7,11), (8,11), (9,11), (8,10), (8,12), (9,10), (9,12))))
    F => 978286106 -> ListBuffer(Island(List((5,1), (6,1), (7,1), (6,0), (6,2), (8,1), (7,2), (8,0), (8,2), (7,3), (9,0), (9,2), (8,3), (7,4), (9,3), (9,4), (9,5), (9,6), (8,6), (9,7), (9,8), (8,8), (7,8), (7,7), (7,9))))
     */

    println(numDistinctIslands2(sea2))
  }


  case class Island(ijs: Seq[(Int, Int)]) {
    val (n,m,grid) = {
      val minI = ijs.map(_._1).min
      val minJ = ijs.map(_._2).min
      val maxI = ijs.map(_._1).max
      val maxJ = ijs.map(_._2).max

      val n = maxI - minI + 1
      val m = maxJ - minJ + 1
      val g = Array.ofDim[Int](n, m)
      ijs.foreach { case (i,j) =>
        g(n-(maxI-i)-1)(m-(maxJ-j)-1) = 1
      }
      (n,m,g)
    }


    def rotations: List[List[List[Int]]] = {
      val move = (0 to 7).map {
        case 0 => (i: Int, j: Int) => (i, j)
        case 1 => (i: Int, j: Int) => (n-1-i, j)
        case 2 => (i: Int, j: Int) => (i, m-1-j)
        case 3 => (i: Int, j: Int) => (n-1-i, m-1-j)
        case 4 => (i: Int, j: Int) => (j, i)
        case 5 => (i: Int, j: Int) => (m-1-j, i)
        case 6 => (i: Int, j: Int) => (j, n-1-i)
        case 7 => (i: Int, j: Int) => (m-1-j, n-1-i)
      }

      (0 to 7).map { id =>
        val (nn, nm) = if (id > 3) (m, n) else (n, m)
        val a = Array.ofDim[Int](nn, nm)
        for (i <- 0 until n) {
          for (j <- 0 until m) {
            val (ni, nj) = move(id)(i, j)
            a(ni)(nj) = grid(i)(j)
          }
        }
        a.toList.map(_.toList)
      }.toList
    }

    override def hashCode(): Int = rotations.map(_.hashCode()).toSet.hashCode()
  }

  def findIslands(grid: Array[Array[Int]]): Seq[Island] = {
    val n = grid.length
    val m = grid(0).length
    val ds = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
    val seen = mutable.Set[(Int, Int)]()

    val islands = ListBuffer[Island]()

    def grabIsland(i: Int, j: Int): Island = {

      val q = new mutable.Queue[(Int, Int)]()
      val ijs = mutable.ListBuffer[(Int, Int)]()
      q.enqueue((i, j))


      while (q.nonEmpty) {
        val (x, y) = q.dequeue()
        if (!seen((x, y))) {
          seen += ((x, y))
          if (grid(x)(y) == 1) {
            ijs.append((x, y))

            ds.foreach { case (di, dj) =>
              val ni = x + di
              val nj = y + dj
              if (inBounds(ni, n) && inBounds(nj, m) && !seen(ni, nj))
                q.enqueue((ni, nj))
            }
          }
        }
      }

      Island(Seq(ijs: _*))
    }

    for (i <- 0 until n) {
      for (j <- 0 until m) {
        if (!seen(i,j)) {
          if (grid(i)(j) == 1) {
            islands.append(grabIsland(i, j))
          }
        }
      }
    }


    islands
  }

  def numDistinctIslands2(grid: Array[Array[Int]]): Int = {
    val y1 = Island(List((0,6), (1,6), (1,7), (2,7)))
    val y2 = Island(List((0,12), (1,12), (0,13), (1,11)))

    println(s"${y1.hashCode() }")
    println(s"${y2.hashCode() }")

    println(s"--------------")
    val islands = findIslands(grid)
    islands.map(_.hashCode()).toSet.size
  }

  def pp(a: Array[Array[Int]]) = println(s"${a.toList.map(_.toList).mkString("\n")}")
}

/*
    private[this] def toColumn(ints: Array[Int]): Array[Array[Int]] = {
      val a = Array.ofDim[Int](1, ints.length)
      for (i <- 0 to ints.length) {
        a(0)(i) = ints(i)
      }
      a
    }

    private[this] def rotate(times: Int): Array[Array[Int]] = {
      var ans = asGrid
      for (_ <- 0 until times-1) {
        ans = ans.map(toColumn(_)).flatten.reverse
      }
      ans
    }

    def rotate90: Array[Array[Int]]    = rotate(1)
    def rotate180: Array[Array[Int]]   = rotate(2)
    def rotate270: Array[Array[Int]]   = rotate(3)
    def reflectHor: Array[Array[Int]]  = null
    def reflectVert: Array[Array[Int]] = null
 */