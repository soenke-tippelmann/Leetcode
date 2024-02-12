package leetcode.medianfinder

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random


class BoundedMedianFinderSpec extends AnyWordSpec with Matchers {

  def A(v: Int, values: Int*): Array[Int] = Array(v, values: _*)

  "MedianFinder" when {

    val finders =
      Table(
        ("name", "finderF"),
        ("naive", MedianFinder.bounded _),
      )

    val specialCases =
      Table(
        ("numbers", "result"),
        (A(0, 0, 1, 1, 1, 2, 0, 2, 2, 0), 1.0),
        (A(0, 0, 0, 1, 1, 2), .5),
        (A(15, 3, 34, 9, 96, 41, 78, 71, 31, 90), 37.5)
      )

    val randomValues =
      Table(
        "numbers",
        Array.tabulate(10)(_ => Random.between(0, 101)),
        Array.tabulate(11)(_ => Random.between(0, 101)),
        Array.tabulate(100)(_ => Random.between(0, 101)),
        Array.tabulate(1000)(_ => Random.between(0, 101)),
        Array.tabulate(10000)(_ => Random.between(0, 101)),
        Array.tabulate(101)(_ => Random.between(0, 101)),
        Array.tabulate(10005)(_ => Random.between(0, 101)),
        Array.tabulate(1)(_ => Random.between(0, 101)),
        Array.tabulate(2)(_ => Random.between(0, 101)),
        Array.tabulate(5)(_ => Random.between(0, 101)),
        Array.tabulate(10)(_ => Random.between(0, 101)),
        Array.tabulate(11)(_ => Random.between(0, 101)),
        Array.tabulate(1001)(_ => Random.between(0, 101)),
        Array.tabulate(12345)(_ => Random.between(0, 101)),
        Array.tabulate(1_000_000)(_ => Random.between(0, 101)),
      )

    forAll(finders) {
      (name: String, finderF: () => MedianFinder) => {
        s"using finder $name" should {
          "compute examples from task description correctly" in {
            val finder = finderF()

            finder.addNum(1)
            finder.addNum(2)
            finder.findMedian shouldEqual 1.5

            finder.addNum(3)
            finder.findMedian shouldEqual 2.0
          }

          "compute fixed examples correctly" in {
            forAll(specialCases) {
              (numbers: Array[Int], result: Double) =>
                println(s"checking ${numbers.mkString("[", ",", "]")}")

                val finder = finderF()
                numbers.foreach(finder.addNum)
                finder.findMedian shouldEqual result
            }
          }

          "compute random challenges correctly" in {
            forAll(randomValues) {
              (numbers: Array[Int]) =>
                // println(s"checking ${numbers.mkString("[", ",", "]")}")
                // println(s"  sorted: ${numbers.sorted.mkString("[", ",", "]")}")

                val finder = finderF()
                numbers.foreach(finder.addNum)
                val result = finder.findMedian

                val naiveFinder = MedianFinder.naive
                numbers.foreach(naiveFinder.addNum)
                val expectedResult = naiveFinder.findMedian

                // println(s"Result: $result, expected result: $expectedResult")
                result shouldEqual expectedResult
            }
          }
        }
      }
    }
  }
}

