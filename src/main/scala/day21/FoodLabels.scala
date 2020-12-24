package day21

case class FoodLabels(ingredients: Map[String, Int] = Map.empty, allergens: Map[String,Allergen] = Map.empty) {

  def add(ingreds: List[String], allgs: Set[Allergen]): FoodLabels = {
    val updatedAllergens = allgs.foldLeft(allergens)((acc, al) => acc + newAllergen(al))
    FoodLabels(newIngredients(ingreds), updatedAllergens)
  }

  def allergenFreeTotal(): Int = allergenFree().values.sum

  def dangerousList(): String = {
    val combos = possibleAllergenCombos()
    if (combos.size > 1)
      throw new RuntimeException("more than one valid combo " + combos)
    else {
      combos.head.toList.sortBy(_.name).map(_.possiblyPresentIn.mkString("")).mkString(",")
    }
  }

  def allergenFree():Map[String,Int] = {
    val possibleAllergens = allergens.values.foldLeft(Set.empty[String])((acc,al) => acc ++ al.possiblyPresentIn)
    ingredients.filter{case (name,_) => !possibleAllergens.contains(name)}
  }

  private def possibleAllergenCombos(): Set[Set[Allergen]] =
    possibleCombos2(allergens.values.toList,Set.empty[Allergen],Set.empty[Set[Allergen]])

  private def possibleCombos2(allergenList: List[Allergen], allergensUsed: Set[Allergen], combosSoFar:Set[Set[Allergen]]):Set[Set[Allergen]] = allergenList match {
    case Nil => combosSoFar + allergensUsed
    case hd::tl => {
      val posIngredients = hd.possiblyPresentIn.filter(ing => !allergensUsed.exists(al => al.possiblyPresentIn.contains(ing)))
      posIngredients.foldLeft(combosSoFar){(combos, ing) =>
        possibleCombos2(tl,allergensUsed + Allergen(hd.name, Set(ing)), combos)
      }
    }
  }

  private def newIngredients(ingredientNames: List[String]):Map[String,Int] =
    ingredients ++ ingredientNames.map { name =>
      name -> (if (ingredients.contains(name)) ingredients(name) + 1 else 1)
    }

  private def newAllergen(allergen: Allergen):(String,Allergen) = {
    if (allergens.contains(allergen.name))
      allergen.name -> Allergen(allergen.name, allergen.possiblyPresentIn intersect allergens(allergen.name).possiblyPresentIn)
    else
      allergen.name -> allergen
  }

}

object FoodLabels {
  def parseLine(line: String):(List[String], Set[Allergen]) = {
    val cleanedLine = line.replaceAll("\\(contains",":").replaceAll("\\)","")
    val splitLine = cleanedLine.split(":")
    val ingredients = splitLine(0).split("\\s").map(_.trim).toList
    val allergenNames = splitLine(1).split("\\s*,").map(_.trim).toSet
    (ingredients, allergenNames.map(name => Allergen(name,ingredients.toSet)))
  }

  def fromInput(input: List[String]):FoodLabels =
    input.foldLeft(FoodLabels())((acc, line) => (acc.add _).tupled(FoodLabels.parseLine(line)))
}