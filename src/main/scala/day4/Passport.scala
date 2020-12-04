package day4

case class Passport(pairs: Map[String, String]) {
  def isValid: Boolean = {
    val requiredKeys = Set("ecl", "hcl", "pid", "iyr", "eyr", "byr", "hgt")
    if (pairs.keys.toSet.intersect(requiredKeys) != requiredKeys) return false

    if (pairs("byr").toInt < 1920 || pairs("byr").toInt > 2002) return false
    if (pairs("iyr").toInt < 2010 || pairs("iyr").toInt > 2020) return false
    if (pairs("eyr").toInt < 2020 || pairs("eyr").toInt > 2030) return false

    val heightReEx = """^([0-9]+)(in|cm)$""".r
    pairs("hgt") match {
      case heightReEx(value,unit) => {
        if (unit == "cm" && (value.toInt < 150 || value.toInt > 193)) return false
        if (unit == "in" && (value.toInt < 59 || value.toInt > 76)) return false
      }
      case _ => return false
    }

    val hairRegEx = """^#([0-9a-f]{6})$""".r
    if (!hairRegEx.matches(pairs("hcl"))) return false

    val eyeColors = Set("amb","blu","brn","gry","grn","hzl","oth")
    if (!eyeColors.contains(pairs("ecl"))) return false

    val pidRegEx = """^([0-9]{9})$""".r
    if (!pidRegEx.matches(pairs("pid"))) return false

    true
  }
}

object Passport {
  def fromPairs(inputString: String): Passport = {
    val pairs = inputString.split(" ")
    val pairMap = pairs.foldLeft(Map.empty[String, String])((acc, pair) => {
      val pairRegEx = """^([a-z]+):(.*)$""".r
      pair match {
        case pairRegEx(key,value) => acc + (key -> value)
        case _ => throw new RuntimeException("cannot match")
      }
    })
    Passport(pairMap)
  }
}