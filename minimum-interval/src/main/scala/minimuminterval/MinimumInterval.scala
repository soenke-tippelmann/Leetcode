package minimuminterval

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
}

