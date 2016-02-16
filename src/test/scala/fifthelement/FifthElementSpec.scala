package fifthelement

import fifthelement.NthElement._
import org.scalatest.{MustMatchers, WordSpec}

class FifthElementSpec extends WordSpec with MustMatchers{

  "FifthElement" when {

    "passed an empty list" should {
      "return None" in {
        fifthElement(List.empty[Int]) mustBe None
      }
    }

    "passed a list smaller than 5 elements" should {
      "return None" in {
        fifthElement(List(1,2,3,4)) mustBe None
      }
    }

    "passed a list containing 5 elements" should {
      "return the first element" in {
        fifthElement(List(1,2,3,4,5)) mustBe Some(1)
      }
    }

    "passed a list containing more than 5 elements" should {
      "return the 5 element from the end of the list" in {
        fifthElement(List(1,2,3,4,5,6,7,8,9)) mustBe Some(5)
      }
    }
  }

}
