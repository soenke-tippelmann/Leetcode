package leetcode.topk

/**
 * LeetCode: https://leetcode.com/problems/top-k-frequent-elements
 */
object TopKNaive {
  def naive(numbers: Array[Int], k: Int): Array[Int] = {
    numbers
      .groupBy(identity)
      .mapValues(_.length)
      .toArray
      .sortBy(_._1)(Ordering.Int.reverse) // just to stabilize the order to compare algorithms in the tests
      .sortBy(_._2)(Ordering.Int.reverse)
      .map(_._1)
      .take(k)
  }
}
