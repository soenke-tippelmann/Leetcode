package leetcode.medianfinder

/**
 * LeetCode: https://leetcode.com/problems/find-median-from-data-stream
 *
 * This implementation assumes that only values between 0 and 100 are picked. This is an additional constraint
 * mentioned at the bottom of the task
 */
class MedianFinderBounded extends MedianFinder {
  private val MIN_BOUND = 0
  private val MAX_BOUND = 100

  private val collector = Array.fill(MAX_BOUND - MIN_BOUND + 1)(0)
  private var counter = 0

  private var currentMedian: Int = Int.MinValue
  private var medianLeftCount: Int = 0

  // the count of the current median in case the same number was added several times
  private def currentMedianOffset = (counter - 1) / 2 - medianLeftCount

  override def addNum(num: Int): Unit = {
    collector(num) = collector(num) + 1
    counter = counter + 1

    if (counter == 1) {
      currentMedian = num
      return
    }

    if (num < currentMedian) {
      medianLeftCount = medianLeftCount + 1
    }

    if (currentMedianOffset < 0) {
      currentMedian = findPrevMedian()
      medianLeftCount = medianLeftCount - collector(currentMedian)
    }
    else if (currentMedianOffset >= collector(currentMedian)) {
      medianLeftCount = medianLeftCount + collector(currentMedian)
      currentMedian = findNextMedian()
    }
  }

  private def findPrevMedian(): Int = {
    var prevMedian = currentMedian
    do {
      prevMedian = prevMedian - 1
    } while (collector(prevMedian) == 0)
    prevMedian
  }

  private def findNextMedian(): Int = {
    var nextMedian = currentMedian
    do {
      nextMedian = nextMedian + 1
    } while (collector(nextMedian) == 0)
    nextMedian
  }

  override def findMedian: Double = {
    if (counter % 2 == 1) {
      return currentMedian
    }

    if (currentMedianOffset >= collector(currentMedian) - 1) {
      val nextMedian = findNextMedian()
      return (currentMedian + nextMedian).toDouble / 2.0
    }

    currentMedian
  }
}
