package leetcode.archerycompetition

import scala.collection.mutable

/**
 * LeetCode: https://leetcode.com/problems/maximum-points-in-an-archery-competition/
 *
 * This version "avoids" recursion by managing its own stack of sub-results that still need to be computed
 */
object ArcheryCompetitionIterative {
  def maximumBobPoints(numArrows: Int, aliceArrows: Array[Int]): Array[Int] = {
    case class R(score: Int, bobArrows: Array[Int])

    val results = mutable.Map.empty[(Int, Int), R]
    val stack = mutable.Stack.empty[(Int, Int)]

    def calculate(idx: Int, availableArrows: Int): Unit = {
      assert(availableArrows >= 0)

      if (idx < 1 || availableArrows < 1) {
        val arr = Array.fill(aliceArrows.length)(0)
        arr.update(0, availableArrows)
        results.put((idx, availableArrows), R(0, arr))
        return
      }

      val numNecessaryArrows = aliceArrows(idx) + 1

      if (!results.contains((idx - 1, availableArrows))) {
        stack.push((idx, availableArrows)) // recompute later
        stack.push((idx - 1, availableArrows))
        return
      }
      val excluded = results((idx - 1, availableArrows))

      if (numNecessaryArrows <= availableArrows) {
        if (!results.contains((idx - 1, availableArrows - numNecessaryArrows))) {
          stack.push((idx, availableArrows)) // recompute later
          stack.push((idx - 1, availableArrows - numNecessaryArrows))
          return
        }
        val r = results((idx - 1, availableArrows - numNecessaryArrows))

        if(r.score + idx > excluded.score) {
          val included = r.copy(score = r.score + idx, bobArrows = r.bobArrows.updated(idx, numNecessaryArrows))
          results.put((idx, availableArrows), included)
          return
        }
      }

      results.put((idx, availableArrows), excluded)
    }

    // do actual calculation
    stack.push((aliceArrows.length - 1, numArrows))

    do {
      val (idx, availableArrows) = stack.pop()
      calculate(idx, availableArrows)
    } while (stack.nonEmpty)

    results((aliceArrows.length - 1, numArrows)).bobArrows
  }
}
