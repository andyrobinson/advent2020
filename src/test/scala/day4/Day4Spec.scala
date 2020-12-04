package day4

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFunSpec with Matchers {
  it("should read the pairs") {
    val passport = Passport.fromPairs("ecl:oth hcl:#733820 cid:124 pid:111220591 iyr:2019 eyr:2001 byr:1933 hgt:159in")
    passport.pairs.keys.toSet shouldBe Set("ecl","hcl","cid","pid","iyr","eyr","byr","hgt")
  }

  it("should report passport as value if it has all eight keys") {
    val passport = Passport.fromPairs("ecl:oth hcl:#733820 cid:124 pid:111220591 iyr:2019 eyr:2021 byr:1933 hgt:59in")
    passport.isValid shouldBe true
  }

  it("should report passport as value if it has all keys except cid") {
    val passport = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    passport.isValid shouldBe true
  }

  it("should report passport as invalid if it does not have all required keys") {
    val passport = Passport.fromPairs("cid:56 byr:1971 hcl:#efcc98 pid:649868696 iyr:2011 eyr:2025 hgt:164cm")
    passport.isValid shouldBe false
  }

  it("should have valid birth year") {
      val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
      val passportTooOld = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1919 iyr:2020 hcl:#888785 hgt:188cm")
      val passportTooYoung = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:2003 iyr:2020 hcl:#888785 hgt:188cm")

      passportGood.isValid shouldBe true
      passportTooOld.isValid shouldBe false
      passportTooYoung.isValid shouldBe false
  }

  it("should have valid issue year") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportIssueOld = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2009 hcl:#888785 hgt:188cm")
    val passportIssueYoung = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:2000 iyr:2021 hcl:#888785 hgt:188cm")

    passportGood.isValid shouldBe true
    passportIssueOld.isValid shouldBe false
    passportIssueYoung.isValid shouldBe false
  }

  it("should have valid expiry year") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportExpiryOld = Passport.fromPairs("ecl:blu pid:117915262 eyr:2031 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportExpiryYoung = Passport.fromPairs("ecl:blu pid:117915262 eyr:2019 byr:2000 iyr:2020 hcl:#888785 hgt:188cm")

    passportGood.isValid shouldBe true
    passportExpiryOld.isValid shouldBe false
    passportExpiryYoung.isValid shouldBe false
  }


  it("should have valid expiry height") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportGood2 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:66in")
    val passportBad1 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:149cm")
    val passportBad2 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:194cm")
    val passportBad3 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:58in")
    val passportBad4 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:77in")
    val passportBad5 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:4cubits")

    passportGood.isValid shouldBe true
    passportGood2.isValid shouldBe true
    passportBad1.isValid shouldBe false
    passportBad2.isValid shouldBe false
    passportBad3.isValid shouldBe false
    passportBad4.isValid shouldBe false
    passportBad5.isValid shouldBe false
  }

  it("should have valid hair color") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportGood2 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#88bcef hgt:188cm")
    val passportBad1 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#8zz785 hgt:188cm")
    val passportBad2 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#88785 hgt:188cm")
    val passportBad3 = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#8ab785a hgt:188cm")

    passportGood.isValid shouldBe true
    passportGood2.isValid shouldBe true
    passportBad1.isValid shouldBe false
    passportBad2.isValid shouldBe false
    passportBad3.isValid shouldBe false
  }

  it("should have valid eye color") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportGood2 = Passport.fromPairs("ecl:gry pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#88bcef hgt:188cm")
    val passportBad1 = Passport.fromPairs("ecl:zan pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#8aa785 hgt:188cm")

    passportGood.isValid shouldBe true
    passportGood2.isValid shouldBe true
    passportBad1.isValid shouldBe false
  }

  it("should have valid passport number") {
    val passportGood = Passport.fromPairs("ecl:blu pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785 hgt:188cm")
    val passportGood2 = Passport.fromPairs("ecl:gry pid:007915262 eyr:2023 byr:1925 iyr:2020 hcl:#88bcef hgt:188cm")
    val passportBad1 = Passport.fromPairs("ecl:blu pid:17915262 eyr:2023 byr:1925 iyr:2020 hcl:#8aa785 hgt:188cm")

    passportGood.isValid shouldBe true
    passportGood2.isValid shouldBe true
    passportBad1.isValid shouldBe false
  }

}
