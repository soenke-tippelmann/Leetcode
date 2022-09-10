package trappingwater

import scala.annotation.tailrec

object TrappingWater {

  case class State(count: Int, blocks: Int, width: Int) {
    def add(other: State): State = State(count + other.count, blocks + other.blocks, width + other.width)

    def extend(height: Int): State = {
      val addCount = width * height - blocks
      val addBlocks = addCount + height
      add(State(addCount, addBlocks, 1))
    }
  }

  object State {
    def empty: State = State(0,0,0)
  }

  def run(input: List[Int]): Int = {
    input match {
      case Nil => 0
      case x :: xs =>
        val (state, reminder) = iterate(xs, x, State.empty, 0)
        run(reminder) + state.count //todo make tail recursive
    }
  }

  // allow iterate to be tail recursive
  private def iterate2 = iterate _

  @tailrec
  private def iterate(input: List[Int], maxHeight: Int, state: State, prevValue: Int): (State, List[Int]) = {

    println(s"input $input maxHeight $maxHeight state $state prevValue $prevValue")

    input match {
      case Nil => (state, Nil)
      case x :: xs if x > maxHeight =>
        (state.extend(maxHeight), x :: xs)
      case x :: xs if x >= prevValue =>
        iterate(xs, maxHeight, state.extend(x), x)
      case x :: xs =>
        val (result, reminder) = iterate2(xs, x, State.empty, 0)
        val combinedState = state.add(result)
        reminder match {
          case Nil => (combinedState, Nil)
          case x :: xs if x > maxHeight =>
            (combinedState.extend(maxHeight), x :: xs)
          case x :: xs if x >= prevValue =>
            iterate(xs, maxHeight, combinedState.extend(x), x)
          case x :: xs =>
            iterate2(xs, maxHeight, result.extend(x).add(state), prevValue)
        }
    }
  }

  def run2(input: List[Int]): Int = {
    val arr = input.toArray
    val rp = arr.size - 1
    iterate3(arr, 0, 0, rp, 0, 0)
  }

  def iterate3(input: Array[Int], leftPointer: Int, leftHeight: Int, rightPointer: Int, rightHeight: Int, count: Int): Int = {

    println(input, leftPointer, leftHeight, rightPointer, rightHeight, count)

    if (leftPointer > rightPointer) {
      count
    } else {
      if( leftHeight < rightHeight ) {
        iterate3(input, leftPointer + 1, Math.max(leftHeight, input(leftPointer)), rightPointer, rightHeight, count + Math.max(0, leftHeight - input(leftPointer)))
      } else {
        iterate3(input, leftPointer, leftHeight, rightPointer - 1, Math.max(rightHeight, input(rightPointer)), count + Math.max(0, rightHeight - input(rightPointer)))
      }
    }
  }
}
