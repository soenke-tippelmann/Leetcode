package kthlargest

import scala.collection.mutable.PriorityQueue

class KthLargest(k: Int, nums: List[Int]) {
  val ordering = Ordering.Int.reverse
  val queue = PriorityQueue(nums.take(k):_*)(ordering)
  nums.drop(k).foreach(add _)

  def add(value: Int): Int = {
    if(queue.head <= value) {
      queue.dequeue()
      queue.addOne(value)
    }
    queue.head
  }
}

