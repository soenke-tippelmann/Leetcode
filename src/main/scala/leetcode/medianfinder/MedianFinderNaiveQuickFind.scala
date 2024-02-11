package leetcode.medianfinder

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * LeetCode: https://leetcode.com/problems/find-median-from-data-stream
 */
class MedianFinderNaiveQuickFind extends MedianFinder {
  private val collector: ListBuffer[Int] = ListBuffer.empty[Int]

  override def addNum(num: Int): Unit = collector.addOne(num)

  override def findMedian: Double = {
    @tailrec
    def iterate(list: Array[Int], leftPad: Int, rightPad: Int): Double = {
      val pivot = list(Random.between(0, list.length))
      val (left, right, leftMin, rightMin) = partition(list, pivot)

      // if only entries of the same value exist, we cannot reduce further and already have found the answer
      if (right.isEmpty && pivot == leftMin) {
        return pivot
      }

      // if we have two equal partitions and the pivot in the middle, we have found the answer
      if (left.length + leftPad - 1 == right.length + rightPad) {
        return pivot
      }

      // if we have two equal partitions, we know that pivot and the one element to the right of the pivot (== rightMin,
      // since pivot == leftMax by definition) need to be averaged
      if (left.length + leftPad == right.length + rightPad) {
        return (pivot + rightMin.head).toDouble / 2.0
      }

      // if the left side contains more elements, we discard the right partition
      if (left.length + leftPad >= right.length + rightPad) {
        iterate(left, leftPad, rightPad + right.length)
      } else { // if the right side contains more elements, discard the left partition
        iterate(right, leftPad + left.length, rightPad)
      }
    }

    iterate(collector.toArray, 0, 0)
  }

  private def partition(array: Array[Int], pivot: Int): (Array[Int], Array[Int], Int, Option[Int]) = {
    val left = Array.newBuilder[Int]
    val right = Array.newBuilder[Int]

    var leftMin: Int = pivot
    var rightMin: Option[Int] = None

    for (number <- array) {
      if (number <= pivot) {
        left.addOne(number)
        leftMin = Math.min(leftMin, number)
      } else {
        right.addOne(number)
        rightMin = Some(Math.min(rightMin.getOrElse(Int.MaxValue), number))
      }
    }

    (left.result(), right.result(), leftMin, rightMin)
  }
}
