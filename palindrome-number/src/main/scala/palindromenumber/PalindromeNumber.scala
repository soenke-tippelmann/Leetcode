package palindromenumber

import scala.annotation.tailrec

object PalindromeNumber {
  def run(number: Int): Boolean =
    if ( number < 0 ) false
    else {
      val highestDigit = Math.floor(Math.log(number) / Math.log(10)).asInstanceOf[Int]
      iterate(number, 0, highestDigit)
    }

  @tailrec
  private def iterate(number: Int, lowPosition: Int, highPosition: Int): Boolean = {
    if( lowPosition > highPosition ) true
    else if ( number < 10 ) true
    else {
      val lowDigit = getDigit(number, lowPosition)
      val highDigit = getDigit(number, highPosition)

      (lowDigit == highDigit) && iterate(number, lowPosition + 1, highPosition - 1)
    }
  }

  private def getDigit(number: Int, position: Int): Int = {
    val positionAsLeadingDigit = number % Math.pow(10, position + 1)
    val isolatedDigit = positionAsLeadingDigit / Math.pow(10, position)
    isolatedDigit.toInt
  }
}

