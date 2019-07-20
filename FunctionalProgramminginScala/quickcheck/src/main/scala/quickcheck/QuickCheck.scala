package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
  for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("meldTwoHeaps") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val h3 = meld(h1, h2)
    val min3 = findMin(h3)
    min3 == min2 || min3 == min1
  }

  property("min2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a1, empty)
    val h2 = insert(a2, h)
    val min2 = findMin(h2)
    min2 == a1 || min2 == a2
  }

  property("constructList") = forAll { (h: H) =>
    def conList(h: H): List[A] = {
      if(isEmpty(h)) Nil
      else findMin(h) :: conList(deleteMin(h))
    }
    val list = conList(h)
    list == list.sorted
  }

  property("insDelMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }
  property("meldAndConstructList") = forAll { (h1: H, h2: H) =>
    def conList(h: H): List[A] = {
      if(isEmpty(h)) Nil
      else findMin(h) :: conList(deleteMin(h))
    }
    val list1 = conList(h1)
    val list2 = conList(h2)
    val list3 = (list1 ++ list2).sorted
    val list4 = conList(meld(h1, h2))
    list4 == list3
  }
}
