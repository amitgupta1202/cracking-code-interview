package linked_lists

import scala.annotation.tailrec
import scala.collection.mutable

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

  def dd_without_modifying(linkedList: Option[LinkedList], lastNodeOfResult: LinkedList, values: mutable.Set[Int]): LinkedList = {
    if (linkedList.isEmpty) lastNodeOfResult
    else {
      if (values(linkedList.get.value)) dd_without_modifying(linkedList.get.next, lastNodeOfResult, values)
      else {
        val newNode = LinkedList(linkedList.get.value)
        lastNodeOfResult.next = Some(newNode)
        dd_without_modifying(linkedList.get.next, newNode, values + linkedList.get.value)
      }
    }
  }

  //Test case
  val ll = LinkedList(1, LinkedList(2, LinkedList(3, LinkedList(3, LinkedList(3, LinkedList(4))))))

  assert(prettyString(ll) == "1 --> 2 --> 3 --> 3 --> 3 --> 4")

  val result = LinkedList(ll.value)
  dd_without_modifying(ll.next, result, mutable.Set.empty)
  assert(prettyString(result) == "1 --> 2 --> 3 --> 4")

  dd(ll)
  assert(prettyString(ll) == "1 --> 2 --> 3 --> 4")
}
