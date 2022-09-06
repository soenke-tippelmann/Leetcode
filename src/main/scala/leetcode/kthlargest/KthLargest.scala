package leetcode.kthlargest

import scala.collection.mutable

class KthLargest(k: Int, nums: List[Int]) {
  val ordering = Ordering.Int.reverse
  val queue = mutable.PriorityQueue(nums.take(k): _*)(ordering)
  nums.drop(k).foreach(add)

  def add(value: Int): Int = {
    if (queue.head <= value) {
      queue.dequeue()
      queue.addOne(value)
    }
    queue.head
  }
}

