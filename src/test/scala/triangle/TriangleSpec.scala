package triangle

import org.scalacheck.{Test, Gen, Properties}
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Triangles._

object TriangleSpec extends Properties("Triangle"){
    import Generators._

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    (1 to 100).foreach( i => println("*" * 100 + p))
    super.overrideParameters(p.withMaxDiscardRatio(20))
  }

  property("validSides") = forAll(scaleneGen){ sides => validateSides(sides)}


  property("equilateral") = forAll(equilateralGen){ sides =>
    triangleType(sides) match {
      case Right(Equilateral) => true
      case _ => false
    }
  }


  property("isosceles") = forAll(isoscelesGen){ sides =>
    triangleType(sides) match {
      case Right(Isosceles) => true
      case x => false
    }
  }

  property("scalene") = forAll(scaleneGen) { sides =>
    triangleType(sides) match
    {
      case Right(Scalene) => true
      case _ => false
    }
  }


  object Generators {

    val ceiling = Integer.MAX_VALUE / 2
    val greaterThanZero = choose(1, ceiling)

    def validTriangleGen(side2Gen: Int => Gen[Int], side3Gen: Int => Int => Gen[Int]) = for {
      side1 <- greaterThanZero
      side2 <- side2Gen(side1)
      side3 <- side3Gen(side1)(side2)
      if side1 + side2 > side3
      if side2 + side3 > side1
      if side3 + side1 > side2
    } yield (side1,side2,side3)

    val equilateralGen = validTriangleGen( side1 => const(side1), side1 => _ => const(side1))
    val isoscelesGen = validTriangleGen(side1 => const(side1), side1 => side2 => choose(1, (side1 + side2) - 1))
    val scaleneGen = validTriangleGen(side1 => choose(side1, Int.MaxValue), side1 => side2 => choose(1, (side1 + side2) - 1))
  }
}
