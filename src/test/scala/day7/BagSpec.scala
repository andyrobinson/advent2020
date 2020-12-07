package day7

import java.security.KeyStore.TrustedCertificateEntry

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BagSpec extends AnyFunSpec with Matchers {
  it("should parse a rule with one bag") {
    val line = "light brown bags contain 5 muted magenta bags."
    val bag = Bag.fromRule(line)
    bag.innerBags shouldBe List((5, "muted magenta"))
    bag.description shouldBe "light brown"
  }

  it("should return the names of inner bags") {
    val line = "light brown bags contain 5 muted magenta bags, 3 shiny red bags."
    val bag = Bag.fromRule(line)

    bag.innerBagNames shouldBe List("muted magenta", "shiny red")
  }

  it("should say if we directly contain a bag") {
    val line = "light brown bags contain 5 muted magenta bags, 3 shiny red bags."
    val bag = Bag.fromRule(line)

    bag.contains("shiny red") shouldBe true
    bag.contains("sticky orange") shouldBe false
  }

  it("should parse a rule with any one bag") {
    val line = "vibrant maroon bags contain 4 light gray bags."
    val bag = Bag.fromRule(line)
    bag.innerBags shouldBe List((4, "light gray"))
    bag.description shouldBe "vibrant maroon"
  }

  it("should parse a rule with more than one bag") {
    val line = "drab lavender bags contain 4 mirrored crimson bags, 3 bright violet bags, 5 posh gold bags, 2 bright olive bags."
    val bag = Bag.fromRule(line)
    bag.innerBags shouldBe List((4, "mirrored crimson"), (3, "bright violet"), (5, "posh gold"), (2, "bright olive"))
    bag.description shouldBe "drab lavender"
  }

  it("should parse a rule with no bags") {
    val line = "plaid green bags contain no other bags."
    val bag = Bag.fromRule(line)
    bag.innerBags shouldBe List.empty
    bag.description shouldBe "plaid green"
  }

}
