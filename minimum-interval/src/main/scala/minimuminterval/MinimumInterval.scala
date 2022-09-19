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

  private def findValue(lookupTree: TreeMap[Int, Int], query: Int): Int =
    lookupTree.get(query).getOrElse(lookupTree.maxBefore(query).get._2)

  private def buildTree(intervals: Array[Interval]): TreeMap[Int, Int] = {
    val tree = TreeMap(0 -> -1)
    intervals.foreach(processInterval(tree, _))
    println(tree)
    tree
  }

  private def processInterval(tree: TreeMap[Int, Int], interval: Interval): Unit = {
    tree.update(interval.right + 1, findValue(tree, interval.right + 1))
    println("  /  ", tree, interval)

    val maxBefore = findValue(tree, interval.left)
    if(maxBefore > interval.size || maxBefore == -1) {
      tree.update(interval.left, interval.size)
    }
    println("  /  ", tree, interval)

    println("range: ", tree.range(interval.left + 1, interval.right + 1))
    tree
      .range(interval.left + 1, interval.right + 1)
      .foldLeft(findValue(tree, interval.left))(
        (prevValue, entry) => updatePosition(tree, interval, entry._1, entry._2, prevValue))

    println("  ->", tree)
  }

  private def updatePosition(tree: TreeMap[Int, Int], interval: Interval, position: Int, value: Int, prevValue: Int): Int = {
    val res =
    if (value <= interval.size && value != -1) {
      value
    } else if (prevValue < interval.size && prevValue != -1) {
      tree.update(position, interval.size)
      interval.size
    } else {
      tree.remove(position)
      prevValue
    }

    println("  /  ", tree, position, value, prevValue)

    res
  }

}

