package rotateimage

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import rotateimage.RotateImage
import scala.reflect.ClassTag

class JumpGameSpec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)

  "Rotate image" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          // test cases from the task definition
          (A(A(1,2,3),A(4,5,6),A(7,8,9)), A(A(7,4,1),A(8,5,2),A(9,6,3))),
          (A(A(5,1,9,11),A(2,4,8,10),A(13,3,6,7),A(15,14,12,16)),
            A(A(15,13,2,5),A(14,3,4,1),A(12,6,8,9),A(16,7,10,11)))
        )

      def toStr[T](value: Array[T]): String = "[" + value.mkString(",") + "]"
      def squareToStr(value: Array[Array[Int]]): String = toStr(value.map(toStr _))

      forAll (values) {
        (input: Array[Array[Int]], result: Array[Array[Int]]) => {
          println(s"\nRunning ${squareToStr(input)} expecting ${squareToStr(result)}")
          RotateImage.rotate(input) shouldEqual result
        }
      }
    }
  }
}

