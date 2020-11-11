package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      elem <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(elem, m)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (h: H) =>
    val (heap, _) = extractData(h)
    val h1 = insert(0, heap)
    val h2 = insert(100, h1)
    findMin(h2) == 0
  }

  property("delMin1") = forAll { (h: H) =>
    val (heap, _) = extractData(h)
    val h1 = insert(0, heap)
    isEmpty(deleteMin(h1))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val h = meld(h1, h2)
    val m = if (isEmpty(h)) 0 else findMin(h)
    m == m1 || m == m2
  }

  property("sorted") = forAll { (h: H) =>
    val prevList = List(1, 6, 4, 9, 2)
    val (heap, _) = extractData(h)
    val h1 = insertData(prevList, heap)
    val (_, list) = extractData(h1)
    list.reverse == prevList.sorted
  }

  def insertData(list: List[Int], heap: H): H = {
    list match {
      case head :: rest => insertData(rest, insert(head, heap))
      case _ => heap
    }
  }

  def extractData(heap: H, list: List[Int] = List.empty[Int]): (H, List[Int]) = {
    if(isEmpty(heap)) (heap, list) else extractData(deleteMin(heap), findMin(heap) :: list)
  }

}
