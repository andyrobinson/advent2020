package day23

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RingSpec extends AnyFunSpec with Matchers {
  it("should be able to make a one element ring") {
    val one = Ring().add(5)
    one.next(5) shouldBe 5
  }

  it("should be able to make a multi element ring") {
    val three = Ring().splice(List(1,2,3))
    val next = three.next(3)
    next shouldBe 1
    three.next(next) shouldBe 2
  }

  it("should be able to remove a length of the ring") {
    val ring = Ring().splice(List(1,2,3,4,5,6,7,8,9))
    val (newRing, removed) = ring.remove(3,3)
    newRing.links shouldBe Map(1->2,2->3,3->7,7->8,8->9,9->1)
    removed shouldBe List(4,5,6)
  }

  it("should be able to insert a list of values into the ring at a specified position") {
    val ring = Ring().splice(List(1,2,3,4,5,6))
    val newRing = ring.splice(List (55,56,57), Some(3))
    newRing.links shouldBe Map(1->2, 2->3, 3->55, 55->56, 56->57, 57->4, 4->5, 5->6, 6->1)

  }
}
