package triangle

import scala.annotation.tailrec


object Triangles {

  sealed trait TriangleType
  case object Equilateral extends TriangleType
  case object Isosceles extends TriangleType
  case object Scalene extends TriangleType


  def triangleType[T : Numeric](sides: (T,T,T)): Either[String, TriangleType] = {
    val sidesSet = Set(sides._1,sides._2, sides._3).toList

    for{
      _ <- validateSides(sides).right
      triangleType <- (sidesSet.length match {
        case 3 => Right(Scalene)
        case 2 => Right(Isosceles)
        case 1 => Right(Equilateral)
      }).right
    } yield triangleType
  }

  /**
    *
    * Uses the Triangle Inequality Theoreom to determine if the sides can form a valid triangle
    * [[https://en.wikipedia.org/wiki/Triangle_inequality]]
    *
    * @param sides Tuple of length representing each side of a triangle
    * @tparam T Numeric type
    * @return Either with error message or Unit
    */
  private def validateSides[T: Numeric](sides: (T,T,T)): Either[String,Unit] = {
    val num = implicitly[Numeric[T]]
    def checkInequality(leftHand1: T, leftHand2: T, rightHand: T) = {
      val sum = num.toDouble(leftHand1) + num.toDouble(leftHand2)
      val rightHandDbl = num.toDouble(rightHand)
      for{
        //Check if numbers are too large
        _ <- {
          if(sum == Double.PositiveInfinity || rightHandDbl == Double.PositiveInfinity)
            Left(s"Sides too large $sides") else Right(())
          }.right
        _ <- (if(sum < rightHandDbl) Left(s"Sides of length $sides do not form a triangle") else Right(())).right
      } yield ()
    }

    val (side1, side2, side3) = sides

    for{
      _ <- checkInequality(side1, side2, side3).right
      _ <- checkInequality(side3, side2, side1).right
      _ <- checkInequality(side1, side3, side2).right
    } yield ()
  }
}
