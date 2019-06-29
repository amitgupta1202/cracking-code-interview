package linked_lists

import scala.annotation.tailrec

object Partition extends App {

  @tailrec
  def partition(ll: Option[LinkedList], p: Int): Unit =
    if (ll.get.next.isEmpty) ()
    else {
      if (ll.get.value < p) {
        val temp = ll.get.value
        ll.get.value = ll.get.next.get.value
        ll.get.next.get.value = temp
        partition(ll.get.next, p)
      } else partition(ll.get.next, p)
    }

  @tailrec
  def partition2(linkedList: Option[LinkedList],
                 partitionAt: Int,
                 leftHead: Option[LinkedList] = None,
                 leftLast: Option[LinkedList] = None,
                 rightHead: Option[LinkedList] = None,
                 rightLast: Option[LinkedList] = None): Option[LinkedList] =
    if (linkedList.isEmpty) {
      rightLast.foreach(_.next = leftHead) //join
      rightHead
    }
    else {
      if (linkedList.get.value < partitionAt) {
          val node = Some(LinkedList(linkedList.get.value))
        if (leftHead.isEmpty) partition2(linkedList.get.next, partitionAt, node, node, rightHead, rightLast)
        else {
          leftLast.foreach(_.next = node)
          partition2(linkedList.get.next, partitionAt, leftHead, node, rightHead, rightLast)
        }
      }
      else {
        val node = Some(LinkedList(linkedList.get.value))
        if (rightHead.isEmpty) partition2(linkedList.get.next, partitionAt, leftHead, leftLast, node, node)
        else {
          rightLast.foreach(_.next = node)
          partition2(linkedList.get.next, partitionAt, leftHead, leftLast, rightHead, node)
        }
      }

    }

  val test = LinkedList(3, LinkedList(5, LinkedList(8, LinkedList(5, LinkedList(9, LinkedList(2, LinkedList(1)))))))
  assert(prettyString(partition2(Some(test), 5).get) == "5 --> 8 --> 5 --> 9 --> 3 --> 2 --> 1")
  partition(Some(test), 5)
  assert(prettyString(test) == "5 --> 8 --> 5 --> 9 --> 2 --> 1 --> 3")
}
