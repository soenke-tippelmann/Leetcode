package jumpgame

import scala.collection.mutable.Map

object JumpGame {
  def naive(input: Array[Int]): Boolean = {

    def iterate(position: Int): Boolean = {
      if(position >= input.size) false
      else if (position == input.size - 1) true
      else List.range(1, input(position) + 1).exists(jumpSize => iterate(position + jumpSize))
    }

    iterate(0)
  }

  // ----------------------------

  def optimized(input: Array[Int]): Boolean = {

    val cache: Map[Int, Boolean] = Map.empty

    def cachedIterate(position: Int): Boolean = {
      cache.getOrElseUpdate(position, iterate(position))
    }

    def iterate(position: Int): Boolean = {
      if(position >= input.size) false
      else if (position == input.size - 1) true
      else List.range(1, input(position) + 1).exists(jumpSize => cachedIterate(position + jumpSize))
    }

    cachedIterate(0)
  }
}

