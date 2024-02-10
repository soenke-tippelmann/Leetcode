package leetcode.carfleet2

/**
 * Relevant LeetCode: https://leetcode.com/problems/car-fleet-ii
 */
object CarFleet2Naive {
  case class Car(position: Int, speed: Int)

  def carFleet(cars: Array[(Int, Int)]): Array[Double] = {
    if (cars.isEmpty)
      return Array.empty

    val carsSorted: Array[Car] = cars.map((Car.apply _).tupled)
    val timings: Array[Double] = Array.ofDim(carsSorted.length)

    for (idx <- List.range(0, carsSorted.length)) {
      val currentCar = carsSorted(idx)
      timings(idx) =
        carsSorted
          .slice(idx, carsSorted.length)
          .map(c => (c.position - currentCar.position).toDouble / (currentCar.speed - c.speed))
          .filter(_.isFinite)
          .filter(_ > 0.0)
          .minOption
          .getOrElse(-1.0)
    }

    timings
  }
}
