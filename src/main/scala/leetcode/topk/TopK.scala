package leetcode.topk

import scala.collection.mutable

/**
 * LeetCode: https://leetcode.com/problems/top-k-frequent-elements
 */
object TopK {
  def run(numbers: Array[Int], k: Int): Array[Int] = {

    val mapOfCounts = mutable.Map.empty[Int, Int]
    for (number <- numbers) {
      mapOfCounts.put(number, mapOfCounts.getOrElse(number, 0) + 1)
    }

    implicit val ordering: Ordering[(Int, Int)] = Ordering.by[(Int, Int), Int](_._2).reverse
    val maxCounts = mutable.PriorityQueue.empty[(Int, Int)]

    for ((value, count) <- mapOfCounts.iterator) {
      if (maxCounts.length < k) {
        maxCounts.enqueue(value -> count)
      } else if (maxCounts.length >= k && maxCounts.head._2 < count) {
        maxCounts.dequeue()
        maxCounts.enqueue(value -> count)
      }
    }

    maxCounts.dequeueAll[(Int, Int)].map(x => x._1).toArray
  }
}
