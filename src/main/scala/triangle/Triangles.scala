package triangle

import scala.annotation.tailrec


object Triangles {

  sealed trait TriangleType
  case object Equilateral extends TriangleType
  case object Isosceles extends TriangleType
  case object Scalene extends TriangleType


  def triangleType[T : Numeric](sides: (T,T,T)): Either[String, TriangleType] = {
    val sidesSet = Set(sides._1,sides._2, sides._3).toList
    sidesSet.length match {
      case _ if !validateSides(sides) => Left(s"Impossible to form triangle with sides of length $sides")
      case 3 => Right(Scalene)
      case 2 => Right(Isosceles)
      case 1 => Right(Equilateral)
    }
  }

  def validateSides[T: Numeric](sides: (T,T,T)): Boolean ={
     val numeric = implicitly[Numeric[T]]
     val sidesSeq = List(sides._1,sides._2, sides._3).map(numeric.toDouble)
     @tailrec
     def loop(sideCombinations: List[Seq[Double]]): Boolean = {
       sideCombinations match {
         case Nil => true
         case List(a,b,c) :: tail => a + b > c  && loop(tail)
       }
     }
     loop(Iterator.continually(sidesSeq).flatten.sliding(3,1).take(3).toSet.toList)
  }
}
