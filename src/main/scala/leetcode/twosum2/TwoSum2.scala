package leetcode.twosum2

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint}

object TwoSum2 {
  def twoSum(numbers: Array[Int], target: Int): (Int, Int) = {
    val maxIndex =
      numbers.search(target - numbers(0)) match {
        case Found(idx) => idx
        case InsertionPoint(idx) => idx - 1
      }

    val minIndex =
      numbers.search(target - numbers(numbers.length - 1)) match {
        case Found(idx) => idx
        case InsertionPoint(idx) => idx
      }

    @tailrec
    def iterate(leftIdx: Int, rightIdx: Int): (Int, Int) = {
      val sum = numbers(leftIdx) + numbers(rightIdx)
      println(leftIdx, rightIdx, sum, target)
      if (sum == target) (leftIdx + 1, rightIdx + 1)
      // we could use binary search to find the next index to check
      // but would that be actually faster? => Depends on the situation
      // so hard to say if we could easily optimize
      else if (sum < target) iterate(leftIdx + 1, rightIdx)
      else iterate(leftIdx, rightIdx - 1)
    }

    iterate(minIndex, maxIndex)
  }
}

