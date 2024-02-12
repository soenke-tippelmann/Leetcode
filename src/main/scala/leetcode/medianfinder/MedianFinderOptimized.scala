package leetcode.medianfinder

import scala.collection.mutable

/**
 * LeetCode: https://leetcode.com/problems/find-median-from-data-stream
 */
class MedianFinderOptimized extends MedianFinder {
  private val topHeap = mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
  private val bottomHeap = mutable.PriorityQueue.empty[Int](Ordering.Int)

  override def addNum(num: Int): Unit = {
    assert(topHeap.headOption.getOrElse(Int.MaxValue) >= bottomHeap.headOption.getOrElse(Int.MinValue))
    assert(Math.abs(topHeap.length - bottomHeap.length) <= 1)

    if (topHeap.headOption.getOrElse(Int.MaxValue) < num) {
      topHeap.addOne(num)
      // In case topHeap already had one more element, we have to move one element down to bottomHeap
      if (topHeap.size - bottomHeap.size > 1) {
        bottomHeap.addOne(topHeap.dequeue())
      }
    } else if (bottomHeap.headOption.getOrElse(Int.MinValue) > num) {
      bottomHeap.addOne(num)
      // In case bottomHeap already had one more element, we have to move one element up to topHeap
      if (topHeap.size - bottomHeap.size < -1) {
        topHeap.addOne(bottomHeap.dequeue())
      }
    } else {
      if (topHeap.size <= bottomHeap.size) {
        topHeap.addOne(num)
      } else {
        bottomHeap.addOne(num)
      }
    }

    assert(topHeap.headOption.getOrElse(Int.MaxValue) >= bottomHeap.headOption.getOrElse(Int.MinValue))
    assert(Math.abs(topHeap.length - bottomHeap.length) <= 1)
  }

  override def findMedian: Double = {
    if (topHeap.length == bottomHeap.length) {
      (topHeap.head + bottomHeap.head).toDouble / 2.0
    } else if (topHeap.length > bottomHeap.length) {
      topHeap.head
    } else {
      bottomHeap.head
    }
  }
}
