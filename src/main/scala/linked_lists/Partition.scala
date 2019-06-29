package linked_lists

object Partition extends App {

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

  val test = LinkedList(3, LinkedList(5, LinkedList(8, LinkedList(5, LinkedList(9, LinkedList(2, LinkedList(1)))))))

  partition(Some(test), 5)
  assert(prettyString(test) == "5 --> 8 --> 5 --> 9 --> 2 --> 1 --> 3")
}
