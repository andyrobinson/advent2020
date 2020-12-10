package day10

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AdaptersSpec extends AnyFunSpec with Matchers {
  it("should report on the differences") {
    val adapters = List(16,10,15,5,1,11,7,19,6,12,4)
    println(adapters.sorted)
    Adapters.findDifferences(adapters) shouldBe (7,5)
  }

  it("should count ones") {
    val adapters = List(3,4,5,6,7,8,9)
    Adapters.findDifferences(adapters) shouldBe (6,2)
  }

  it("should count threes and add one") {
    val adapters = List(1,4,7,10,13,16,19)
    Adapters.findDifferences(adapters) shouldBe (1,7)
  }

  it("should return numbers within 3 of target number") {
    Adapters.routesWithin3(List(PathCount(0,0)), 1) shouldBe List(PathCount(0,0))
    Adapters.routesWithin3(List(PathCount(39,0),PathCount(40,0),PathCount(42,0)), 43) shouldBe List(PathCount(40,0),PathCount(42,0))
  }

  it("should count a one element path") {
    Adapters.countPaths(List(1)) shouldBe 1
  }

  it("should count single paths of any length") {
    val adapters = List(3,6,9)
    Adapters.countPaths(adapters) shouldBe 1
  }

  it("should count paths where there are multiple endings") {
    val adapters = List(3,4,5) // (0),3,4,5,(8)|(0),3,5,(8)
    Adapters.countPaths(adapters) shouldBe 2
  }

  it("should count paths where there are multiple branches") {
    val adapters = List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19)
    Adapters.countPaths(adapters) shouldBe 8
  }

}
