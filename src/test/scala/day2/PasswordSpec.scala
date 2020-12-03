package day2

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PasswordSpec extends AnyFunSpec with Matchers {
  it("should split a line into useful parts") {
    val password = Password.parse("3-15 v: kfvrbtsxlmsqvzgbdk")
    password.point1 shouldBe 3
    password.point2 shouldBe 15
    password.limitedCharacter shouldBe 'v'
    password.password shouldBe "kfvrbtsxlmsqvzgbdk"
  }

  it("should throw if no match") {
    assertThrows[RuntimeException]{
      Password.parse("fkjhgkjdfhg")
    }
  }

  it("should have valid countWithinRange if the number of repeated characters are within the range") {
    val password = Password.parse("2-4 a: aaabbc")
    password.countWithinRange shouldBe true
  }

  it("should not have valid countWithinRange if the number of repeated characters is less than the range") {
    val password = Password.parse("1-3 z: aaabbc")
    password.countWithinRange shouldBe false
  }

  it("should not have valid countWithinRange if the number of repeated characters is more than the range") {
    val password = Password.parse("3-5 k: aakakbkbkkkc")
    password.countWithinRange shouldBe false
  }

  it("should have valid xorPresent if the first character matches") {
    val password = Password.parse("1-13 z: zsgfzqpbbzjndvz")
    password.xorPresent shouldBe true
  }

  it("should have valid xorPresent if the second character matches") {
    val password = Password.parse("3-5 b: aaakbc")
    password.xorPresent shouldBe true
  }

  it("should not have valid xorPresent if neither character matches") {
    val password = Password.parse("3-5 j: aakakbkbkkkc")
    password.xorPresent shouldBe false
  }

  it("should not have valid xorPresent if both characters match") {
    val password = Password.parse("3-5 j: zhjhjhtyrurtui")
    password.xorPresent shouldBe false

  }

  it("should have valid xorPresent even if an index is out of range") {
    val password = Password.parse("3-20 j: qqj")
    password.xorPresent shouldBe true
  }

}
