package leetcode.carfleet2

import leetcode.carfleet2.CarFleet2Functional.ImplicitExtensions.ListExtension

import scala.annotation.tailrec

/**
 * Relevant LeetCode: https://leetcode.com/problems/car-fleet-ii
 */
object CarFleet2Functional {
  case class Car(position: Int, speed: Int)

  def carFleet(cars: Array[(Int, Int)]): Array[Double] = {
    if (cars.isEmpty)
      return Array.empty

    val carsSorted: Array[Car] = cars.map((Car.apply _).tupled)

    @tailrec
    def iterate(idx: Int, stack: List[Car], timings: List[Double]): List[Double] = {
      if (idx < 0) return timings

      val currentCar = carsSorted(idx)

      val cleanedStack = stack.discardWhile(_.speed >= currentCar.speed)

      if (cleanedStack.isEmpty) {
        iterate(idx - 1, currentCar :: cleanedStack, -1.0 :: timings)
      } else {
        val (minTiming, minIdx) =
          cleanedStack
            .map(c => (c.position - currentCar.position).toDouble / (currentCar.speed - c.speed))
            .zipWithIndex
            .min

        iterate(idx - 1, currentCar :: cleanedStack.takeRight(minIdx + 1), minTiming :: timings)
      }
    }

    iterate(carsSorted.length - 1, List.empty, List.empty).toArray
  }

  // ----------- Utility Extensions ---------------
  object ImplicitExtensions {
    implicit class ListExtension[T](lst: List[T]) {
      @tailrec
      final def discardWhile(pred: T => Boolean): List[T] = {
        if (lst.isEmpty) lst
        else if (pred(lst.head)) lst.tail.discardWhile(pred)
        else lst
      }
    }
  }
}
