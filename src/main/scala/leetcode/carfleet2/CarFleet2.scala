package leetcode.carfleet2

import scala.collection.mutable

/**
 * Relevant LeetCode: https://leetcode.com/problems/car-fleet-ii
 */
object CarFleet2 {
  case class Car(position: Int, speed: Int)

  def carFleet(cars: Array[(Int, Int)]): Array[Double] = {
    if (cars.isEmpty)
      return Array.empty

    val carsSorted: Array[Car] = cars.map((Car.apply _).tupled)
    val timings: Array[Double] = Array.ofDim(carsSorted.length)

    val stack = mutable.Stack.empty[Car]

    for (idx <- List.range(0, carsSorted.length).reverse) {

      val currentCar = carsSorted(idx)

      stack.popWhile(_.speed >= currentCar.speed)

      if (stack.isEmpty) {
        timings(idx) = -1.0
      }
      else {
        val (minTiming, minIdx) =
          stack
            .map(c => (c.position - currentCar.position).toDouble / (currentCar.speed - c.speed))
            .zipWithIndex
            .min

        timings(idx) = minTiming
        stack.drop(stack.length - minIdx)
      }
      stack.push(currentCar)
    }

    timings
  }
}
