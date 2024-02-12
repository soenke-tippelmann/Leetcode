package leetcode.medianfinder

import scala.collection.mutable.ListBuffer

/**
 * LeetCode: https://leetcode.com/problems/find-median-from-data-stream
 */
class MedianFinderNaive extends MedianFinder {
  private val collector: ListBuffer[Int] = ListBuffer.empty[Int]

  override def addNum(num: Int): Unit = collector.addOne(num)

  override def findMedian: Double = {
    val sorted = collector.sorted

    if (sorted.length % 2 == 1) {
      val medianIndex = sorted.length / 2
      sorted(medianIndex).toDouble
    } else {
      val medianIndex = sorted.length / 2
      (sorted(medianIndex - 1) + sorted(medianIndex)).toDouble / 2.0
    }
  }
}
