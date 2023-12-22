package leetcode.urlifyinplace

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class UrlifyInPlaceSpec extends AnyWordSpec with Matchers {

  "UrlifyInPlace" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          ("", ""),
          ("   ", "%20"),
          ("      ", "%20%20"),
          ("abc", "abc"),
          ("abc ", "abc "),
          ("abc  ", "abc  "),
          (" abc  ", "%20abc"),
          ("  abc    ", "%20%20abc"),
          ("  abc     ", "%20%20abc "),
          (" a bc     ", "%20a%20bc "),
          ("a b c    ", "a%20b%20c"),
          ("Mr John Smith    ", "Mr%20John%20Smith"),
        )

      forAll(values) {
        (input: String, result: String) => {
          println(s"\nRunning '$input' expecting '$result'")
          val array = input.toArray
          UrlifyInPlace.run(array)
          array shouldEqual result.toArray
        }
      }
    }

    "countSpaces" should {
      "compute count correctly" in {
        val values =
          Table(
            ("input", "result"),
            ("", 0),
            (" ", 0),
            ("abc", 0),
            ("abc ", 0),
            ("abc  ", 0),
            (" abc ", 1),
            ("  abc", 2),
            (" a bc ", 2),
            ("a b c ", 2),
            ("Mr John Smith    ", 2)
          )

        forAll(values) {
          (input: String, result: Int) => {
            println(s"\nRunning '$input' expecting '$result'")
            val array = input.toArray
            UrlifyInPlace.countSpaces(array)._1 shouldEqual result
          }
        }
      }

      "compute last non whitespace position correctly" in {
        val values =
          Table(
            ("input", "result"),
            ("", None),
            (" ", None),
            ("abc", Some(2)),
            ("abc ", Some(2)),
            ("abc  ", Some(2)),
            (" abc ", Some(3)),
            ("  abc", Some(4)),
            (" a bc ", Some(4)),
            ("a b c ", Some(4)),
            ("Mr John Smith    ", Some(12))
          )

        forAll(values) {
          (input: String, result: Option[Int]) => {
            println(s"\nRunning '$input' expecting '$result'")
            val array = input.toArray
            UrlifyInPlace.countSpaces(array)._2 shouldEqual result
          }
        }
      }
    }
  }
}
