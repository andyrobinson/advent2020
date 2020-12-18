package day19

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class RulesSpec extends AnyFunSpec with Matchers {
  it("should read a single character rule") {
    Rule.fromLine("""92: "b"""") shouldBe Rule(92,Set(SimpleMatch("b")))
  }

  it("should read a single sub rule") {
    Rule.fromLine("8: 42") shouldBe Rule(8, Set(SubRuleSeq(List(42))))
  }

  it("should read two rules") {
    Rule.fromLine("10: 11 12") shouldBe Rule(10, Set(SubRuleSeq(List(11,12))))
  }

  it("should read alternative sets of rules") {
    Rule.fromLine("10: 11 12 | 13 14") shouldBe Rule(10, Set(SubRuleSeq(List(11,12)),SubRuleSeq(List(13,14))))
    Rule.fromLine("20: 2 | 12 15") shouldBe Rule(20, Set(SubRuleSeq(List(2)),SubRuleSeq(List(12,15))))
    Rule.fromLine("30: 456 4 | 88") shouldBe Rule(30, Set(SubRuleSeq(List(456,4)),SubRuleSeq(List(88))))
    Rule.fromLine("44: 5 | 92") shouldBe Rule(44, Set(SubRuleSeq(List(5)),SubRuleSeq(List(92))))
  }

  it("should read into a map of rules") {
    Rules.fromInput(List("1: 2 3 | 3 2","2: 3 3","""3: "b"""","","some other stuff we ignore")) shouldBe Map (
      1 -> Rule(1, Set(SubRuleSeq(List(2,3)),SubRuleSeq(List(3,2)))),
      2 -> Rule(2, Set(SubRuleSeq(List(3,3)))),
      3 -> Rule(3, Set(SimpleMatch("b")))
    )
  }

  it("should read patterns into a list") {
    Rules.patternsFromInput(List("1: 2 3 | 3 2","","ababababa","abbbbaab")) shouldBe List("ababababa","abbbbaab")
  }

  it("should match a simple character") {
    val rules = Rules.fromInput(List("""0: "b""""))
    Rules.matching(List("a","b","bb"),rules) shouldBe Set("b")
  }

  it("should match longer strings") {
    val rules = Map(0 -> Rule(0, Set(SimpleMatch("baa"), SimpleMatch("bbaa"), SimpleMatch("bbbbbbb"))))
    Rules.matching(List("ba","baa","baaa", "baaab","bbaaa","bbaa"),rules) shouldBe Set("baa", "bbaa")
  }

  it("should use rules to match") {
    val rules = Rules.fromInput(List("0: 2 3 | 3 2","""2: "b"""","""3: "a""""))
    Rules.matching(List("ba","ab","baa", "abb","bbaaa"),rules) shouldBe Set("ab", "ba")
  }

  it("should use subrules to match") {
    val rules = Rules.fromInput(List("0: 1 2", "1: 3 4 | 4 3","2: 4 3 | 4 4", """3: "b"""","""4: "a""""))
    Rules.matching(List("ba","ab","abab","abaa","baab","baaa","baabb","ababab"),rules) shouldBe Set("abab","abaa","baab","baaa")
  }

}
