package leetcode.rotateimage

import scala.annotation.tailrec

object RotateImage {
  def rotate(matrix: Array[Array[Int]]): Unit = {
    val maxIndex = matrix.length - 1
    println(maxIndex, maxIndex / 2)
    for {
      row <- List.range(0, maxIndex / 2 + 1)
      column <- List.range(row, maxIndex - row)
    } exchange(matrix, row, column)
  }

  private def exchange(matrix: Array[Array[Int]], row: Int, col: Int): Unit = {
    val size = matrix.length

    val width = size - 1 - 2 * row

    val (r0, c0) = getTransformedIdx(size, row, col, 0)
    val (r1, c1) = getTransformedIdx(size, row, col, 3 * width)
    val (r2, c2) = getTransformedIdx(size, row, col, 2 * width)
    val (r3, c3) = getTransformedIdx(size, row, col, 1 * width)

    val temp = matrix(r0)(c0)
    matrix(r0)(c0) = matrix(r1)(c1)
    matrix(r1)(c1) = matrix(r2)(c2)
    matrix(r2)(c2) = matrix(r3)(c3)
    matrix(r3)(c3) = temp
  }

  private def getTransformedIdx(size: Int, row: Int, col: Int, idx: Int): (Int, Int) = {
    val (r, c) = getIdx(size - 2 * row, 0, col - row, idx)
    (r + row, c + row)
  }

  @tailrec
  private def getIdx(size: Int, row: Int, col: Int, idx: Int): (Int, Int) = {
    if (col < size - 1 && col > 0) {
      getIdx(size, row, 0, idx + col)
    } else {

      val (rowFactor, colFactor) = (row, col) match {
        case x if x == (0, 0) => (0, 1)
        case x if x == (size - 1, 0) => (-1, 0)
        case x if x == (0, size - 1) => (1, 0)
        case x if x == (size - 1, size - 1) => (0, -1)
      }

      if (idx >= size) getIdx(size, row + rowFactor * (size - 1), col + colFactor * (size - 1), idx - size + 1)
      else (row + rowFactor * idx, col + colFactor * idx)
    }
  }
}

