import scala.annotation.tailrec

package object linked_lists {
  class LinkedList(var value: Int, var next: Option[LinkedList])

  object LinkedList {
    def apply(value: Int, next: LinkedList): LinkedList = new LinkedList(value, Some(next))
    def apply(value: Int): LinkedList = new LinkedList(value, None)
  }

  @tailrec
  def prettyString(ll: LinkedList, s: List[String] = List.empty): String =
    if (ll.next.isEmpty)
      (s :+ ll.value.toString).mkString(" --> ")
    else
      prettyString(ll.next.get, s :+ ll.value.toString)

}
