package day21

case class FoodLabels(ingredients: Map[String, Int] = Map.empty, allergens: Map[String,Allergen] = Map.empty) {

  def add(ingreds: Set[String], allergens: Set[Allergen]): FoodLabels = {
    val updatedAllergens = allergens.foldLeft(Map.empty[String,Allergen])((acc,al) => acc ++ newAllergens(al))
    val reducedAllergens = reduceUnqiue(updatedAllergens)
    FoodLabels(newIngredients(ingreds), reducedAllergens)
  }

  def addIngredients(ingredients:Set[String]): FoodLabels = {
    FoodLabels(newIngredients(ingredients), allergens)
  }

  def allergenFree():Map[String,Int] = {
    val possibleAllergens = allergens.values.foldLeft(Set.empty[String])((acc,al) => acc ++ al.possiblyPresentIn)
    ingredients.filter{case (name,count) => !possibleAllergens.contains(name)}
  }

  def allergenFreeTotal(): Int = allergenFree().values.sum

  private def reduceUnqiue(allergens: Map[String, Allergen], reduced: Set[String] = Set.empty): Map[String, Allergen] = {
    val toReduce: Set[String] = allergens.values.filter(_.possiblyPresentIn.size == 1).map(_.name).toSet diff reduced
    toReduce.foldLeft(allergens){(ac, allergenName) =>
      val reducedAllergens = ac.map{
        case pr if pr._1 == allergenName => pr // don't reduce yourself!
        case (key, al) => (key -> al.copy(possiblyPresentIn = al.possiblyPresentIn - allergens(allergenName).possiblyPresentIn.head))}
      reduceUnqiue(reducedAllergens, reduced + allergenName)
    }
  }

  private def newIngredients(ingredientNames: Set[String]):Map[String,Int] = {
    ingredients ++ ingredientNames.map { name =>
      name -> (if (ingredients.contains(name)) ingredients(name) + 1 else 1)
    }
  }

  private def newAllergens(allergen: Allergen) = {
    if (allergens.contains(allergen.name)) {
      allergens + (allergen.name -> Allergen(allergen.name, allergen.possiblyPresentIn intersect allergens(allergen.name).possiblyPresentIn))
    } else {
      allergens + (allergen.name -> allergen)
    }
  }

}

object FoodLabels {
  def parseLine(line: String):(Set[String], Set[Allergen]) = {
    val cleanedLine = line.replaceAll("\\(contains",":").replaceAll("\\)","")
    val splitLine = cleanedLine.split(":")
    val ingredients = splitLine(0).split("\\s").map(_.trim).toSet
    val allergenNames = splitLine(1).split("\\s*,").map(_.trim).toSet
    (ingredients, allergenNames.map(name => Allergen(name,ingredients)))
  }

  def fromInput(input: List[String]):FoodLabels =
    input.foldLeft(FoodLabels())((acc, line) => (acc.add _).tupled(FoodLabels.parseLine(line)))
}