package subset

import org.scalatest.{MustMatchers, Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen._
import Subset._

class SubsetSpec extends PropSpec with PropertyChecks with MustMatchers {
  import Generators._

  property("subset must correctly determine when list1 is a subset of list2"){
    forAll(subsetGen){ case (list1, list2, list3) =>
      subset(list1,list2) mustBe true
      subset(list3,list2) mustBe false
      subset(Nil, list2)  mustBe true
      subset(list2, Nil)  mustBe false


      subsetToo(list1,list2)  mustBe true
      subsetToo(list3,list2)  mustBe false
      subsetToo(Nil,list2)    mustBe true
      subsetToo(list2, Nil)   mustBe false

    }
  }

  property("subset must correctly handle Nil case"){
    subset(Nil,Nil)     mustBe true
    subsetToo(Nil,Nil)  mustBe true
  }


  object Generators{

    val subsetGen = for{
      list1 <- nonEmptyListOf(choose(1,100))
      list2 <- nonEmptyListOf(choose(1,100))
      list3 <- nonEmptyListOf(choose(101,200))
    } yield (list1, list2 ::: list1, list3)

  }
}
