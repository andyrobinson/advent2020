package day25

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CryptoSpec extends AnyFunSpec with Matchers {
  it("should calculate a loop size of 8 for the card") {
    Crypto.findLoopSize(5764801) shouldBe 8
  }

  it("should calculate a loop size of 11 for the card") {
    Crypto.findLoopSize(17807724) shouldBe 11
  }

  it("should calculate the key from the loop size and the public key") {
    Crypto.findEncryptionKey(5764801L, 17807724L) shouldBe 14897079
  }

}
