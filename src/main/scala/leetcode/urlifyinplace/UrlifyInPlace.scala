package leetcode.urlifyinplace

import scala.annotation.tailrec

/**
 * Objective: Replace each occurence of ' ' in an Array[Char] with %20 in place, ignoring trailing whitespace.
 * Assumption: The array has at least as many trailing whitespace to hold the modified string. (Additional trailing
 *    whitespace will just be ignored)
 * Runtime: O(n) / Linear in the length of the array
 * Space: O(1)
 */
object UrlifyInPlace {
  def run(input: Array[Char]): Array[Char] = {
    val (numSpaces, lastNonWhiteIndex) = countSpaces(input)

    if (lastNonWhiteIndex.isEmpty) {
      replaceWhitespace(input, input.length / 3 - 1, input.length / 3)
    } else {
      replaceWhitespace(input, lastNonWhiteIndex.get, numSpaces)
    }

    input
  }

  def replaceWhitespace(input: Array[Char], lastNonWhiteIndex: Int, numSpaces: Int): Unit = {
    @tailrec
    def iterate(position: Int, offset: Int): Unit = {
      if (position < 0) {
        ()
      } else if (input(position) == ' ') {
        input(position + offset) = '0'
        input(position + offset - 1) = '2'
        input(position + offset - 2) = '%'
        iterate(position - 1, offset - 2)
      } else {
        input(position + offset) = input(position)
        iterate(position - 1, offset)
      }
    }

    iterate(lastNonWhiteIndex, 2 * numSpaces)
  }

  /**
   * @return a tuple of (count of spaces, index of last non whitespace)
   */
  def countSpaces(input: Array[Char]): (Int, Option[Int]) = {
    @tailrec
    def iterate(position: Int, isTrailing: Boolean, count: Int, lastNonWhiteIndex: Option[Int]): (Int, Option[Int]) =
      if (position < 0) {
        (count, lastNonWhiteIndex)
      } else {
        (input(position), isTrailing) match {
          case (' ', true) => iterate(position - 1, isTrailing = true, count = 0, lastNonWhiteIndex = None)
          case (_, true) => iterate(position - 1, isTrailing = false, count = 0, lastNonWhiteIndex = Some(position))
          case (' ', false) => iterate(position - 1, isTrailing = false, count + 1, lastNonWhiteIndex)
          case (_, false) => iterate(position - 1, isTrailing = false, count, lastNonWhiteIndex)
        }
      }

    iterate(position = input.length - 1, isTrailing = true, count = 0, lastNonWhiteIndex = None)
  }
}
