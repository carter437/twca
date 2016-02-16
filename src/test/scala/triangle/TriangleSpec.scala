package triangle

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import triangle.Triangles._

import scala.util.Right

class TriangleSpec extends PropSpec with PropertyChecks with Matchers{
    import Generators._

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 100, maxDiscarded = 1000)


  property("triangleType must fail for invalid triangle values"){
    forAll(invalidTriangleGen){ sides => triangleType(sides).isLeft shouldBe true}
  }

  property("triangleType must return Equilateral for valid equilateral sides"){
    forAll(equilateralGen){ sides =>
      triangleType(sides) shouldBe Right(Equilateral)
    }
  }

  property("triangleType must return Isosceles for valid isosceles sides"){
    forAll(isoscelesGen) { sides =>
      triangleType(sides) shouldBe Right(Isosceles)
    }
  }

  property("triangleType must return Scalene for valid scalene sides"){
    forAll(scaleneGen) { sides =>
      triangleType(sides) shouldBe Right(Scalene)
    }
  }

  object Generators {

    val ceiling = ( Double.MaxValue / 5 ) - 1
    val greaterThanZero = choose(1D, ceiling)
    def validTriangleCheck(side1: Double, side2: Double, side3: Double) = {
      side1 + side2 >= side3 &&
      side2 + side3 >= side1 &&
      side3 + side1 >= side2
    }

    def validTriangleGen(side2Gen: Double => Gen[Double], side3Gen: Double => Double => Gen[Double]) = for {
      side1 <- greaterThanZero
      side2 <- side2Gen(side1)
      side3 <- side3Gen(side1)(side2)
      if validTriangleCheck(side1,side2, side3)
    } yield (side1,side2,side3)

    val invalidTriangleGen = for{
      side1 <- Arbitrary.arbitrary[Double]
      side2 <- Arbitrary.arbitrary[Double]
      side3 <- Arbitrary.arbitrary[Double]
      if !validTriangleCheck(side1,side2,side3)
    } yield (side1, side2, side3)

    val equilateralGen = validTriangleGen( side1 => const(side1), side1 => _ => const(side1))
    val isoscelesGen = validTriangleGen(side1 => const(side1), side1 => side2 => choose(1, (side1 + side2) - 1))
    val scaleneGen = validTriangleGen(side1 => choose(side1, ceiling), side1 => side2 => choose(1, (side1 + side2) - 1))
  }
}
