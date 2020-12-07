package day7

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LuggageRulesSpec extends AnyFunSpec with Matchers {
  it("should return a list with a simple contains") {
    val luggageRules = LuggageRules(List("bright chartreuse bags contain 2 wavy blue bags.",
    "wavy blue bags contain 2 yukky green bags.",
      "yukky green bags contain no other bags."))

    luggageRules.bagsContaining("wavy blue") shouldBe Set("bright chartreuse")
  }

  it("should return a list with chains of contains") {
    val luggageRules = LuggageRules(List("bright chartreuse bags contain 2 wavy blue bags.",
      "wavy blue bags contain 2 bright pink bags.",
      "bright pink bags contain 2 yukky green bags.",
      "plain brown bags contain 2 dull green bags.",
      "dull green bags contain 2 yukky green bags.",
      "yukky green bags contain no other bags."))

    luggageRules.bagsContaining("yukky green") shouldBe Set("bright chartreuse", "wavy blue", "bright pink", "plain brown", "dull green")
  }

  it("should count recursively") {
    val luggageRules = LuggageRules(List("bright chartreuse bags contain 2 wavy blue bags.",
      "wavy blue bags contain 2 bright pink bags, 1 dull green bag.",
      "bright pink bags contain 2 yukky green bags.",
      "yukky green bags contain no other bags.",
      "dull green bags contain no other bags."))

    luggageRules.bagsInside("bright chartreuse") shouldBe 2 + (2 * 3) + (2 * 2 * 2)

  }

}
