package minimuminterval

import scala.collection.mutable.TreeMap
import scala.annotation.tailrec

object MinimumInterval {
  case class Interval(left: Int, right: Int) {
    val size = right - left + 1
    def contains(value: Int) = value >= left && value <= right
    override def toString = s"[$left,$right]"
  }

  def naive(intervals: Array[Interval], queries: Array[Int]): Array[Int] =
    queries
      .map( query => {
        intervals
          .filter(_.contains(query))
          .map(_.size)
          .minOption
          .getOrElse(-1)
      })

  // -------------------------------------------

  def optimized(intervals: Array[Interval], queries: Array[Int]): Array[Int] = {
    val lookupTree = buildTree(intervals)
    queries.map(findValue(lookupTree, _))
  }

  def buildTree(intervals: Array[Interval]): TreeMap[Int, Int] = {
    val tree = TreeMap(0 -> -1)

    @tailrec
    def iterate(intervals: List[Interval]): Unit = {
      intervals match {
        case Nil => ()
        case interval :: xs =>
          val maxBeforeRight = findValue(tree, interval.right + 1)

          tree.update(interval.right + 1, maxBeforeRight)

          val maxBefore = findValue(tree, interval.left)

          if(maxBefore > interval.size || maxBefore == -1) {
            tree.update(interval.left, interval.size)
          }

          val entries = tree.range(interval.left + 1, interval.right + 1).toList

          @tailrec
          def it2(entries: List[(Int, Int)], prevValue: Int): Unit = {
            entries match {
              case Nil => ()
              case (position, value) :: xs if value <= interval.size && value != -1 =>
                it2(xs, value)
              case (position, value) :: xs if prevValue < interval.size && prevValue != -1 =>
                tree.update(position, interval.size)
                it2(xs, interval.size)
              case (position, _) :: xs =>
                tree.remove(position)
                it2(xs, prevValue)
            }
          }

          it2(entries, findValue(tree, interval.left))

          iterate(xs)
      }
    }

    iterate(intervals.toList)

    println(tree)

    tree
  }

  def findValue(lookupTree: TreeMap[Int, Int], query: Int): Int =
    lookupTree.get(query).getOrElse(lookupTree.maxBefore(query).get._2)
}

