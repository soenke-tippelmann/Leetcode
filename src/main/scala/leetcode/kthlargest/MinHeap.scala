package leetcode.kthlargest

class MinHeap {
  override def toString = toList.mkString(",")

  def toList: List[Int] = array.take(end).toList

  private var maxSize = 1
  private var end = 0
  private var array: Array[Int] = Array.ofDim(maxSize)

  def isEmpty = end < 1
  def minOption: Option[Int] = if (isEmpty) None else Some(array(0))
  def min: Int = minOption.get

  def delete(): Unit = {
    if (isEmpty) return
    end -= 1
    if (end > 0) {
      array(0) = array(end)
    }
    reversePositionElement()
  }

  private def reversePositionElement(): Unit = {
    var position = 0
    var shouldContinue = true
    while (shouldContinue) {
      val leftChildPosition = 2 * position + 1
      val rightChildPosition = 2 * position + 2

      if (leftChildPosition > end) return

      var checkPosition = leftChildPosition
      if (rightChildPosition <= end && array(leftChildPosition) > array(rightChildPosition)) {
        checkPosition = rightChildPosition
      }

      shouldContinue = ensureIntegrity(position, checkPosition)
      position = checkPosition
    }
  }

  def add(value: Int): Unit = {
    ensureArraySize()
    array(end) = value
    positionElement()
    end += 1
  }

  private def ensureArraySize() = {
    if (array.length == end) {
      array = Array.copyOf(array, 2 * array.length)
    }
  }

  private def positionElement() = {
    var position = end
    var shouldContinue = true
    while (shouldContinue && position > 0) {
      val parentPosition = Math.floor((position - 1) / 2).toInt
      shouldContinue = ensureIntegrity(parentPosition, position)
      position = parentPosition
    }
  }

  // Assumption: pos1 < pos2
  private def ensureIntegrity(pos1: Int, pos2: Int): Boolean = {
    val shouldSwap = array(pos1) >= array(pos2)
    if (shouldSwap) {
      swap(pos1, pos2)
    }
    shouldSwap
  }

  private def swap(pos1: Int, pos2: Int) = {
    val temp = array(pos1)
    array(pos1) = array(pos2)
    array(pos2) = temp
  }
}

object MinHeap {
  def empty = new MinHeap()

  def apply(values: Int*) = {
    val heap = empty
    values.foreach(heap.add)
    heap
  }
}

