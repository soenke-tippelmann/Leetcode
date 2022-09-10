package nqueens

import scala.collection.mutable.{ Set => mSet }

object Nqueens {
  def setToStringRep(size: Int, queens: List[Set[(Int, Int)]]) = {
    queens
      .map(
        positions =>
          List.tabulate(size, size)(
            (x,y) => if (positions.contains((x,y))) "Q" else "."
          )
      )
      .map( _.map( _.mkString ) )
  }

  class Counter {
    var count = 0
    def inc(): Unit = count += 1
    def get = count
  }

  def run(size: Int): List[List[String]] = {
    optimized(size)
  }


  // --------------------------------

  def naive(size: Int): List[List[String]] = {
    val iterateCounter = new Counter

    def iterate(queens: Set[(Int, Int)]): List[Set[(Int,Int)]] = {
      iterateCounter.inc()

      if (queens.size == size) {
        List(queens)
      } else {
        findFreeSpots(queens)
          .flatMap(spot => iterate(queens + spot))
          .distinct
      }
    }

    def findFreeSpots(queens: Set[(Int, Int)]): List[(Int, Int)] = {
      for {
        x <- List.range(0,size)
        y <- List.range(0,size)
        if !intersectsQueen(queens, x, y)
      } yield (x, y)
    }

    def intersectsQueen(queens: Set[(Int, Int)], x: Int, y: Int) =
      queens.exists {
        case (qx, qy) =>
          x == qx || y == qy || (qx - x) == (qy - y) || -(qx - x) == (qy - y)
      }

    //main call
    val result = iterate(Set.empty)
    println(s"iterateCounter (naive) ${iterateCounter.get}")
    setToStringRep(size, result)
  }


  // --------------------------------

  def optimized(size: Int): List[List[String]] = {
    val iterateCounter = new Counter

    def iterate(queens: Set[(Int, Int)], lastQueen: (Int, Int)): List[Set[(Int,Int)]] = {
      iterateCounter.inc()
     // println("Iterate", queens)

      val remainingQueens = size - queens.size

      if (remainingQueens == 0) {
      //  println("Done")
        List(queens)
      } else {
        findFreeSpots(queens)
          .filter(_._1 > lastQueen._1)
          .filter(spot => (size - spot._1) >= remainingQueens)
          .flatMap(spot => iterate(queens + spot, spot))
      }
    }

    def findFreeSpots(queens: Set[(Int, Int)]): List[(Int, Int)] = {
      val queensX = queens.map(_._1)
      val queensY = queens.map(_._2)
      for {
        x <- List.range(0,size).filterNot(queensX.contains)
        y <- List.range(0,size).filterNot(queensY.contains)
        if !intersectsQueen(queens, x, y)
      } yield (x, y)
    }

    def intersectsQueen(queens: Set[(Int, Int)], x: Int, y: Int) =
      queens.exists {
        case (qx, qy) =>
          x == qx || y == qy || (qx - x) == (qy - y) || -(qx - x) == (qy - y)
      }

    //main call
    val result = iterate(Set.empty, (-1, -1))
    println(s"iterateCounter (optimized) ${iterateCounter.get}")
    setToStringRep(size, result)
  }
}

