import scala.annotation.tailrec
import scala.collection.mutable

package object linked_lists {
  class LinkedList(var value: Int, var next: Option[LinkedList])

  object LinkedList {
    def apply(value: Int, next: LinkedList): LinkedList = new LinkedList(value, Some(next))
    def apply(value: Int): LinkedList = new LinkedList(value, None)
  }

  @tailrec
  def prettyString(ll: LinkedList, s: StringBuilder = new mutable.StringBuilder()): String =
    if (ll.next.isEmpty) s.append(ll.value).mkString(" --> ")
    else prettyString(ll.next.get, s.append(ll.value))
}
