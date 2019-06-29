package linked_lists

import scala.annotation.tailrec

object DeleteMiddleNode extends App {

  //assumption at least has 3 nodes
  @tailrec
  def deleteMiddleNode(ll: LinkedList, ll2: LinkedList): Unit =
    if (ll2.next.isEmpty || ll2.next.get.next.isEmpty) ll.next = ll.next.get.next
    else deleteMiddleNode(ll.next.get, ll2.next.get.next.get)

  //Test case
  val ll = LinkedList(1, LinkedList(2, LinkedList(3, LinkedList(4, LinkedList(5, LinkedList(6, LinkedList(7)))))))

  deleteMiddleNode(ll, ll.next.get)
  assert(prettyString(ll) == "1 --> 2 --> 3 --> 5 --> 6 --> 7")
  deleteMiddleNode(ll, ll.next.get)
  assert(prettyString(ll) == "1 --> 2 --> 3 --> 6 --> 7")

}
