package jumpgame

import scala.collection.mutable.Map

object JumpGame {

  class Counter {
    var count = 0
    def inc(): Unit = count += 1
    def get = count
  }

  // ----------------------------

  def naive(input: Array[Int]): Boolean = {
    val iterateCounter = new Counter

    def iterate(position: Int): Boolean = {
      iterateCounter.inc()

      if(position >= input.size) false
      else if (position == input.size - 1) true
      else List.range(1, input(position) + 1).exists(jumpSize => iterate(position + jumpSize))
    }

    val result = iterate(0)
    println(s"iterateCounter (naive) ${iterateCounter.get}")
    result
  }

  // ----------------------------

  def optimized(input: Array[Int]): Boolean = {
    val iterateCounter = new Counter
    val cache: Map[Int, Boolean] = Map.empty

    def cachedIterate(position: Int): Boolean = {
      cache.getOrElseUpdate(position, iterate(position))
    }

    def iterate(position: Int): Boolean = {
      iterateCounter.inc()

      if(position >= input.size) false
      else if (position == input.size - 1) true
      else List.range(1, input(position) + 1).exists(jumpSize => cachedIterate(position + jumpSize))
    }

    val result = cachedIterate(0)
    println(s"iterateCounter (optimized) ${iterateCounter.get}")
    result
  }

  // ----------------------------

  def optimized2(input: Array[Int]): Boolean = {
    val iterateCounter = new Counter
    val cache: Map[Int, Boolean] = Map.empty

    def cachedIterate(position: Int): Boolean = {
      cache.getOrElseUpdate(position, iterate(position))
    }

    def iterate(position: Int): Boolean = {
      iterateCounter.inc()

      if(position >= input.size) false
      else if (position == input.size - 1) true
      else List.range(input(position), 0, -1).exists(jumpSize => cachedIterate(position + jumpSize))
    }

    val result = cachedIterate(0)
    println(s"iterateCounter (optimized2) ${iterateCounter.get}")
    result
  }
}

