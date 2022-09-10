package nqueens

object Nqueens {
  def run(size: Int): List[List[String]] = {
    naive(size)
  }

  def naive(size: Int): List[List[String]] = {

    def iterate(queens: Set[(Int, Int)]): List[Set[(Int,Int)]] = {
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

    def setToStringRep(queens: List[Set[(Int, Int)]]) =
      queens
        .map(
          positions =>
            List.tabulate(size, size)(
              (x,y) => if (positions.contains((x,y))) "Q" else "."
            )
        )
        .map( _.map( _.mkString ) )

    //main call
    setToStringRep(iterate(Set.empty))
  }
}

