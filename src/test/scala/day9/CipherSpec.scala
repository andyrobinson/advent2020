package day9

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CipherSpec extends AnyFunSpec with Matchers {
  it("should determine if a sum is present in the list") {
    val parts = List(2L, 3L, 5L, 7L, 11L)
    Cipher.containsSum(14L, parts) shouldBe true
    Cipher.containsSum(4L, parts) shouldBe false
    Cipher.containsSum(19L, parts) shouldBe false
  }

  it("should loop through the list of numbers until an invalid number is found") {
    val preamble = List(2L, 3L, 5L, 7L, 11L)
    val factors = List(5L, 8L, 10L, 33L, 99L)
    Cipher.findInvalid(preamble, factors) shouldBe 33L
  }


  it("should find a contiguous range making a sum") {
    val toSearch = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18).map(_.toLong)
    Cipher.findRange(46, toSearch) shouldBe List(10L,11L,12L,13L)
    Cipher.findRange(20, toSearch) shouldBe List(2L,3L,4L,5L,6L)
  }

}
