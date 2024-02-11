package leetcode.topk

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.ClassTag
import scala.util.Random

class TopKSpec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values: _*)

  "Top K" should {
    "compute correctly" in {
      val values =
        Table(
          ("numbers", "k", "result"),
          // test cases from the task definition
          (A(1, 1, 1, 2, 2, 3), 2, A(1, 2)),
          (A(1), 1, A(1))
        )

      def toStr[T](value: Array[T]): String = "[" + value.mkString(",") + "]"

      forAll(values) {
        (numbers: Array[Int], k: Int, result: Array[Int]) => {
          println(s"\nRunning ${toStr(numbers)} with k=$k expecting ${toStr(result)}")
          TopKNaive.naive(numbers, k) should contain theSameElementsAs result
          TopK.run(numbers, k) should contain theSameElementsAs result
          TopKOptimized.optimized(numbers, k) should contain theSameElementsAs result
        }
      }

      val randomValues =
        Table(
          ("numbers", "k"),
          (Array.tabulate(10)(_ => Random.nextInt() % 2 - 1), 5),
          (Array.tabulate(100)(_ => Random.nextInt() % 2 - 1), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 10),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 20),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 50),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 1000)
        )

      forAll(randomValues) {
        (numbers: Array[Int], k: Int) => {
          println(s"\nRunning random sequence of size ${numbers.length} with k=$k")
          TopKNaive.naive(numbers, k) should contain theSameElementsAs TopKOptimized.optimized(numbers, k)
        }
      }

      forAll(randomValues) {
        (numbers: Array[Int], k: Int) => {
          println(s"\nRunning random sequence of size ${numbers.length} with k=$k")

          val topKResult = TopK.run(numbers, k)

          // it can happen that the result contains less elements than k, hence we cannot just check against k,
          // but have to check against an actual computation
          topKResult.length shouldEqual TopKNaive.naive(numbers, k).length

          // If the result is not unique, then two versions of the algorithm might not agree on the exact answer, since
          // it is indeterministic. Therefore, we have to include all numbers that have a count equal to the lowest
          // count included in the topK result.
          val orderedCounts = numbers.groupBy(identity).view.mapValues(_.length).toArray.sortBy(_._2)(Ordering.Int.reverse)
          val topKminCount = orderedCounts.take(k).reverse.head._2

          val mustContainAll = orderedCounts.filter(x => x._2 > topKminCount).map(x => x._1)
          val mustContainSubset = orderedCounts.filter(x => x._2 == topKminCount).map(x => x._1)

          topKResult should contain allElementsOf mustContainAll // ensure all higher counts are correctly included
          topKResult should contain atLeastOneElementOf mustContainSubset // ensure the min count is somehow included
          (mustContainAll ++ mustContainSubset) should contain allElementsOf topKResult // ensure no wrong results are included
        }
      }
    }
  }
}

