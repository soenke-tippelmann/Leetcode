package topk

object TopK {
  def naive(numbers: Array[Int], k: Int): Array[Int] = {
    numbers
      .groupBy(identity)
      .mapValues(_.size)
      .toArray
      .sortBy(_._2)(Ordering.Int.reverse)
      .map(_._1)
      .take(k)
  }
}

