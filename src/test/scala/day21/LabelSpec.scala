package day21

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LabelSpec extends AnyFunSpec with Matchers {

  it("should read Allergens and ingredients from a line of text") {
    val line = "fhdjf djhfh uuuuf jhdfjhf (contains nuts, sesame)"
    FoodLabels.parseLine(line) shouldBe
      (Set("fhdjf","djhfh","uuuuf","jhdfjhf"),
      Set(Allergen("nuts", Set("fhdjf","djhfh","uuuuf","jhdfjhf")), Allergen("sesame", Set("fhdjf","djhfh","uuuuf","jhdfjhf"))))
  }

  it("should parse when it's a single allergen") {
    val line = "aaa bbb (contains nuts)"
    FoodLabels.parseLine(line) shouldBe
      (Set("aaa","bbb"), Set(Allergen("nuts", Set("aaa","bbb"))))
  }

  it("should consolidate possible allergen ingredients when adding") {
    val ingreds1 = List("aaa", "bbb", "ccc")
    val allergen1 = Allergen("nuts", ingreds1.toSet)
    val foods = FoodLabels().add(ingreds1, Set(allergen1))
    foods.allergens shouldBe Map(allergen1.name -> allergen1)

    val ingreds2 = List("bbb", "ccc", "ddd")
    val foods2 = foods.add(ingreds2, Set(Allergen("nuts", ingreds2.toSet)))
    foods2.allergens shouldBe Map("nuts" -> Allergen("nuts", Set("bbb","ccc")))
  }

  it("should read all the food labels from the input") {
    val input = List("fhdjf djhfh uuuuf jhdfjhf (contains nuts, sesame)",
      "fhdjf djhfh riugh gjfijg (contains fish)",
      "pleodk hhhh uuuuf jhdfjhf (contains dairy, nuts)")

    val labels = FoodLabels.fromInput(input)

    labels.ingredients shouldBe Map("fhdjf" ->2, "djhfh" -> 2, "uuuuf" -> 2, "jhdfjhf" -> 2, "riugh" -> 1, "gjfijg" -> 1,  "pleodk" -> 1, "hhhh" -> 1)
    labels.allergens shouldBe Map(
      "nuts" -> Allergen("nuts",Set("uuuuf", "jhdfjhf")),
      "sesame" -> Allergen("sesame",Set("fhdjf", "djhfh", "uuuuf", "jhdfjhf")),
      "fish" -> Allergen("fish",Set("fhdjf", "djhfh", "riugh", "gjfijg")),
      "dairy" -> Allergen("dairy",Set("pleodk", "hhhh", "uuuuf", "jhdfjhf")))
  }

  it("should produce a list of non allergen ingredients") {
    val input = List("fhdjf djhfh uuuuf jhdfjhf (contains nuts, sesame)",
      "fhdjf djhfh riugh zzzddd (contains dairy)",
      "pleodk uuuuf zzzddd (contains dairy, nuts)")

    val labels = FoodLabels.fromInput(input)

    labels.allergenFree shouldBe Map("pleodk" -> 1)

  }
}
