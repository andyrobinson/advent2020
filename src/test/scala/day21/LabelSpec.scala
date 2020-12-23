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

  it("should count ingredients when adding") {
      val foods = FoodLabels().addIngredients(Set("aaa","bbb","ccc"))
      foods.ingredients shouldBe Map(("aaa" -> 1),("bbb" ->1),("ccc" ->1))

      val foods2 = foods.addIngredients(Set("bbb","ccc","ddd"))
      foods2.ingredients shouldBe Map(("aaa" -> 1),("bbb" ->2),("ccc" ->2),("ddd" ->1))
  }

  it("should consolidate possible allergen ingredients when adding") {
    val ingreds1 = Set("aaa", "bbb", "ccc")
    val allergen1 = Allergen("nuts", ingreds1)
    val foods = FoodLabels().add(ingreds1, Set(allergen1))
    foods.allergens shouldBe Map(allergen1.name -> allergen1)

    val ingreds2 = Set("bbb", "ccc", "ddd")
    val foods2 = foods.add(ingreds2, Set(Allergen("nuts", ingreds2)))
    foods2.allergens shouldBe Map("nuts" -> Allergen("nuts", Set("bbb","ccc")))
  }

  it("should remove an ingredient from other Allergens once it has been associated uniquely")  {
    val allergen1 = Allergen("nuts", Set("aaa", "bbb", "ccc"))
    val allergen2 = Allergen("dairy", Set("bbb", "ccc","eee"))
    val allergen3 = Allergen("nuts", Set("ccc","ddd","eee"))

    val foods = FoodLabels().add(Set.empty, Set(allergen1)).add(Set.empty,Set(allergen2)).add(Set.empty, Set(allergen3))
    foods.allergens shouldBe Map (
      "nuts" -> Allergen("nuts", Set("ccc")),
      "dairy" -> Allergen("dairy", Set("bbb", "eee"))
    )
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
