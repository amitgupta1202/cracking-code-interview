package linked_lists

import scala.annotation.tailrec

/*
  Write code to remove duplicates from an unsorted linked list

  FOLLOW UP

  How would you solve this problem if a temporary buffer is not allowed
 */
object RemoveDuplicates extends App {

  @tailrec
  def dd(ll: LinkedList): Unit =
    if (ll.next.isEmpty) ()
    else {
      if (ll.value == ll.next.get.value) {
        ll.value = ll.next.get.value
        ll.next = ll.next.get.next
        dd(ll)
      } else {
        dd(ll.next.get)
      }
    }

  //Test case
  val ll = LinkedList(1, LinkedList(2, LinkedList(3, LinkedList(3, LinkedList(3, LinkedList(4))))))

  assert(prettyString(ll) == "1 --> 2 --> 3 --> 3 --> 3 --> 4")
  dd(ll)
  assert(prettyString(ll) == "1 --> 2 --> 3 --> 4")
}
