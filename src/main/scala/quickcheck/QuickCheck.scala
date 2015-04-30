package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap{

  //the min of a heap with 1 element should be the element itself
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

//If you insert any two elements into an empty heap,
// finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll{ (a: Int, b: Int) =>
    val heap = insert(b, insert(a, empty))
    findMin(heap) == (if (a< b) a else b)
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("isEmpty") = forAll{ a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }


  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  property("isSorted") = forAll{ heap: H =>
    def checkIsSorted(h: H): Boolean = {
      if (isEmpty(h))
        return true
      else {
        val newHeap = deleteMin(h)
        (isEmpty(newHeap) || (findMin(h) <= findMin(newHeap))) && checkIsSorted(newHeap)
      }
    }
    checkIsSorted(heap)
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min3") = forAll{ (h1: H, h2: H) =>

    if (isEmpty(h1)){
      if (isEmpty(h2)) true
      else
        findMin(h2) == findMin(meld(h1, h2))
    } else {
      if (isEmpty(h2)) findMin(h1) == findMin(meld(h1, h2))
      else
        min(findMin(h1), findMin(h2)) == findMin(meld(h1, h2))
    }
  }

  //checks that two non empty heaps are melded correctly
  property("isMelded") = forAll { (h1: H, h2:H) =>
    def isEqual(h1: H, h2:H): Boolean ={
      if(isEmpty(h1) && isEmpty(h2)) true
      else
        findMin(h1) == findMin(h2) && isEqual(deleteMin(h1), deleteMin(h2))
    }
    isEqual(meld(h1,h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency(
      (1, const(empty)),
      (9, genHeap))
  } yield insert(n, h)



  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
