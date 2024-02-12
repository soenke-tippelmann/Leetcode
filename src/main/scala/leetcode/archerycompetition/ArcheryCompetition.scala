package leetcode.archerycompetition

/**
 * LeetCode: https://leetcode.com/problems/maximum-points-in-an-archery-competition/
 */
object ArcheryCompetition {
  def maximumBobPoints(numArrows: Int, aliceArrows: Array[Int]): Array[Int] = {

    case class R(score: Int, bobArrows: Array[Int])

    def iterate(idx: Int, availableArrows: Int): R = {
      if (idx < 1 || availableArrows < 1) {
        val arr = Array.fill(aliceArrows.length)(0)
        arr.update(0, availableArrows)
        return R(0, arr)
      }

      val numNecessaryArrows = aliceArrows(idx) + 1

      val included =
        if (numNecessaryArrows <= availableArrows) {
          val r = iterate(idx - 1, availableArrows - numNecessaryArrows)
          Some(r.copy(score = r.score + idx, bobArrows = r.bobArrows.updated(idx, numNecessaryArrows)))
        } else None

      val excluded = iterate(idx - 1, availableArrows)

      included
        .filter(_.score > excluded.score)
        .getOrElse(excluded)
    }

    iterate(aliceArrows.length - 1, numArrows).bobArrows
  }
}
