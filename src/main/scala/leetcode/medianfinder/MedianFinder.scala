package leetcode.medianfinder

/**
 * LeetCode: https://leetcode.com/problems/find-median-from-data-stream
 */
trait MedianFinder {
  def addNum(num: Int): Unit

  def findMedian: Double
}

object MedianFinder {
  def naive = new MedianFinderNaive()

  def naiveQuickFind = new MedianFinderNaiveQuickFind()

  def optimized = new MedianFinderOptimized()

  def bounded = new MedianFinderBounded()
}

