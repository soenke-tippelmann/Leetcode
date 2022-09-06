package leetcode.trappingwater

import scala.annotation.tailrec

object TrappingWater {

  def run(input: List[Int]): Int = {
    val arr = input.toArray
    val rightPointer = arr.length - 1
    iterate(arr, 0, 0, rightPointer, 0, 0)
  }

  @tailrec
  private def iterate(input: Array[Int], leftPointer: Int, leftHeight: Int, rightPointer: Int, rightHeight: Int, count: Int): Int = {

    println(input.mkString("Array(", ", ", ")"), leftPointer, leftHeight, rightPointer, rightHeight, count)

    if (leftPointer > rightPointer) {
      count
    } else {
      if (leftHeight < rightHeight) {
        val newLeftHeight = Math.max(leftHeight, input(leftPointer))
        iterate(input, leftPointer + 1, newLeftHeight, rightPointer, rightHeight, count + Math.max(0, leftHeight - input(leftPointer)))
      } else {
        val newRightHeight = Math.max(rightHeight, input(rightPointer))
        iterate(input, leftPointer, leftHeight, rightPointer - 1, newRightHeight, count + Math.max(0, rightHeight - input(rightPointer)))
      }
    }
  }
}
