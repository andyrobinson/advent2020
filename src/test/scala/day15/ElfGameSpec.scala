package day15

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ElfGameSpec extends AnyFunSpec with Matchers {
  it("should calculate the given spoken number") {
    ElfGame.nthNumberSpoken(List(0,3,6),4) shouldBe 0
    ElfGame.nthNumberSpoken(List(0,3,6),5) shouldBe 3
    ElfGame.nthNumberSpoken(List(0,3,6),6) shouldBe 3
    ElfGame.nthNumberSpoken(List(0,3,6),7) shouldBe 1
    ElfGame.nthNumberSpoken(List(0,3,6),8) shouldBe 0
    ElfGame.nthNumberSpoken(List(0,3,6),9) shouldBe 4
    ElfGame.nthNumberSpoken(List(0,3,6),10) shouldBe 0
  }

  it("should calculate the 2020th number as given in the puzzle") {
    ElfGame.nthNumberSpoken(List(1,3,2),2020) shouldBe 1
    ElfGame.nthNumberSpoken(List(2,1,3),2020) shouldBe 10
    ElfGame.nthNumberSpoken(List(1,2,3),2020) shouldBe 27
    ElfGame.nthNumberSpoken(List(2,3,1),2020) shouldBe 78
    ElfGame.nthNumberSpoken(List(3,2,1),2020) shouldBe 438
    ElfGame.nthNumberSpoken(List(3,1,2),2020) shouldBe 1836

  }

  it("should calculate to 30000000th number ") {
    // each one takes about 20s with conventional map, 4s with LongMap
    ElfGame.nthNumberSpoken(List(0,3,6), 30000000) shouldBe 175594
    ElfGame.nthNumberSpoken(List(1,3,2), 30000000) shouldBe 2578
    ElfGame.nthNumberSpoken(List(2,1,3), 30000000) shouldBe 3544142
    ElfGame.nthNumberSpoken(List(1,2,3), 30000000) shouldBe 261214
    ElfGame.nthNumberSpoken(List(2,3,1), 30000000) shouldBe 6895259
    ElfGame.nthNumberSpoken(List(3,2,1), 30000000) shouldBe 18
    ElfGame.nthNumberSpoken(List(3,1,2), 30000000) shouldBe 362
  }

}
