package subset

import scala.annotation.tailrec

object Subset {
  /**
    *
    * @param list1 The list to determine if a subset
    * @param list2 The list to check if it contains a subset
    * @return true if list2 contains a subset of list1
    */
     def subset[T](list1: List[T], list2: List[T]) = {
       (list1, list2) match {
         case (Nil,_) => true
         case (_, Nil) => false
         case _ if list1.length <= list2.length => list2.intersect(list1) == list1
         case _ => false
       }


      list1.length <= list2.length && list1.intersect(list2) == list1
  }


  /**
    *
    * Same as above. Wrote this one in case [[subset]] relied too heavily on the standard library
    *
    * This function recurses on each element in list1 and checks if it is in list2. This is done using the
    * span function on [[scala.collection.immutable.List]]. Span takes from the list until the predicate supplied
    * does not match.  The result is a list of two lists. The left List is the elements that were taken while
    * the predicate applied, the right list is the remainder of the list.
    *
    * By recursing using tail for list1 and for list2 using the left list from the result of span appended with the
    * tail from the right list from the result of span, the head of list1 is effectively removed from list2.
    */
     def subsetToo[T](list1:List[T], list2: List[T]): Boolean = {
       @tailrec
       def loop(l1: List[T], l2: List[T]): Boolean = {
         (l1, l2) match {
           case (Nil, _) => true
           case (head::_, Nil) => false
           case (head::tail, _) => {
            l2.span(_ != head) match {
              case (_ , Nil) => false
              case (before,found::rest) => loop(tail, before ::: rest)
            }
           }
         }
       }

      list2.length >= list1.length && loop(list1,list2)
     }
}
