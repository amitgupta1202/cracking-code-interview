package linked_lists

import scala.annotation.tailrec

/*
  Implement an algorithm to find the kth to last element of a singly linked list
 */
object ReturnKthToLast extends App {

  //O(n)
  @tailrec
  def kth(ll: Option[LinkedList], k: Int): Option[LinkedList] =
    if (ll.isEmpty) None
    else if (k == 1) Some(ll.get)
    else kth(ll.get.next, k - 1)


  //O(n)
  //assumption list at least has k elements
  @tailrec
  def kthFomLast(ll: Option[LinkedList], kthElement: Option[LinkedList], k: Int): Option[LinkedList] =
    if (ll.isEmpty)
      kthElement
    else if (k == 0)
      kthFomLast(ll.get.next, kthElement.get.next, 0)
    else
      kthFomLast(ll.get.next, kthElement, k - 1)

  //Test case
  val ll = LinkedList(1, LinkedList(2, LinkedList(3, LinkedList(4, LinkedList(5, LinkedList(6))))))

  assert(kth(Some(ll), 3).get.value == 3)
  assert(kthFomLast(Some(ll), Some(ll), 3).get.value == 4)

}
