package fifthelement

import scala.annotation.tailrec

object NthElement {
  def nthElement[T](nth: Int)(list: TraversableOnce[T]) : Option[T] = {
      require(nth > 0)

      @tailrec
      def loop(workingList: TraversableOnce[T], lastNth: List[T] = Nil) : Option[T] = {
        workingList match {
          case Nil => lastNth.drop(nth - 1).headOption
          case head::tail => loop(tail, (head :: lastNth).take(nth))
        }

      }
      loop(list)
  }


  def fifthElement[T](list: TraversableOnce[T]) = nthElement(5)(list)
}
