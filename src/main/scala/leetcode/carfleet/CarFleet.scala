package leetcode.carfleet

import scala.annotation.tailrec

/**
 * Relevant LeetCode: https://leetcode.com/problems/car-fleet
 */
object CarFleet {
  case class Car(position: Int, speed: Int)

  def carFleet(target: Int, position: Array[Int], speed: Array[Int]): Int = {
    if (position.isEmpty || position.length != speed.length || target < 1)
      return 0

    val carsSorted: List[Car] =
      position
        .zip(speed)
        .map((Car.apply _).tupled)
        .sortBy(car => -car.position)
        .toList

    @tailrec
    def iterate(fleets: Int, minCar: Car, cars: List[Car]): Int = {
      if (cars.isEmpty) fleets
      else {
        val currentCar = cars.head
        if (currentCar.speed > minCar.speed) {
          iterate(fleets, minCar, cars.tail)
        } else {
          iterate(fleets + 1, currentCar, cars.tail)
        }
      }
    }

    iterate(1, carsSorted.head, carsSorted.tail)
  }
}

