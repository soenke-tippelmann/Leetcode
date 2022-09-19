package jumpgame

import scala.annotation.tailrec

object JumpGame {
  def run(input: Array[Int]): Boolean = iterate(input, 0)

  private def iterate(input: Array[Int], position: Int): Boolean = {
    if(position >= input.size) false
    else if (position == input.size - 1) true
    else List.range(1, input(position) + 1).exists(jumpSize => iterate(input, position + jumpSize))
  }
}

