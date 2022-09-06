package leetcode.kthlargest

class KthLargestWithCustomHeap(k: Int, nums: List[Int]) {
  val heap = MinHeap(nums.take(k): _*)
  nums.drop(k).foreach(add)

  def add(value: Int): Int = {
    if (heap.min <= value) {
      heap.delete()
      heap.add(value)
    }
    heap.min
  }
}
