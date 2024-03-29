package leetcode.topk

import scala.collection.mutable.{Map => mMap, Set => mSet}

/**
 * LeetCode: https://leetcode.com/problems/top-k-frequent-elements
 */
object TopKOptimized {
  class Element(var count: Int, var pred: Option[Element], var succ: Option[Element], var numbers: mSet[Int])

  def optimized(numbers: Array[Int], k: Int): Array[Int] = {
    val lookup: mMap[Int, Element] = mMap.empty

    var minimum = new Element(1, None, None, mSet.empty)
    var maximum = minimum

    def addNumber(number: Int): Unit = {
      val elementO = lookup.get(number)

      if (elementO.isEmpty) {
        if (minimum.count > 1) {
          val newMinimum = new Element(1, None, Some(minimum), mSet.empty)
          minimum.pred = Some(newMinimum)
          minimum = newMinimum
        }
        lookup.addOne(number, minimum)
        minimum.numbers.addOne(number)
      } else {
        val element = elementO.get

        if (element.numbers.size == 1) {
          if (element.succ.isDefined && element.succ.get.count == element.count + 1) {
            element.succ.get.pred = element.pred
            element.succ.get.numbers.addOne(number)
            lookup.update(number, element.succ.get)
            if (element.pred.isDefined) {
              element.pred.get.succ = element.succ
            }
            if (minimum == element) {
              minimum = element.succ.get
            }
          } else {
            element.count += 1
          }
        } else {
          if (element.succ.isDefined && element.succ.get.count == element.count + 1) {
            element.numbers.subtractOne(number)
            element.succ.get.numbers.addOne(number)
            lookup.update(number, element.succ.get)
          } else {
            val newSucc = new Element(element.count + 1, Some(element), element.succ, mSet(number))
            if (element.succ.isDefined) {
              element.succ.get.pred = Some(newSucc)
            }
            element.succ = Some(newSucc)
            element.numbers.subtractOne(number)
            lookup.update(number, newSucc)
            if (newSucc.succ.isEmpty) {
              maximum = newSucc
            }
          }
        }
      }
    }

    def readTopK(count: Int, element: Element): Array[Int] = {
      if (count > 0) {
        if (element.pred.isDefined) {
          element.numbers.toArray.sorted(Ordering.Int.reverse).take(count) ++ readTopK(count - element.numbers.size, element.pred.get)
        } else {
          element.numbers.toArray
        }
      } else {
        Array.empty
      }
    }

    numbers.foreach(addNumber)

    readTopK(k, maximum)
  }
}

