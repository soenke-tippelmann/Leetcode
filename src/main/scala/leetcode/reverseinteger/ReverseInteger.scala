package leetcode.reverseinteger

import scala.annotation.tailrec

object ReverseInteger {
  def run(number: Int): Int = {
    if (number == Int.MinValue) 0
    else if (number < 0) -iterate(-number, 0)
    else iterate(number, 0)
  }

  @tailrec
  private def iterate(number: Int, result: Int): Int = {
    if (number == 0) {
      return result
    }

    val digit = number % 10
    val remainder = number / 10

    val maximum = Int.MaxValue / 10
    if (result > maximum || 10 * result > Int.MaxValue - digit) {
      return 0
    }

    iterate(remainder, 10 * result + digit)
  }
}

