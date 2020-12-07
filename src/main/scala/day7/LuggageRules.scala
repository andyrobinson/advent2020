package day7

case class LuggageRules(input: List[String]) {

  val rules = input.map {line =>
    val b = Bag.fromRule(line)
    b.description -> b
  }.toMap

  def bagsContaining(name: String, bagNames: List[String] = rules.keys.toList): Set[String] = {
    bagNames.foldLeft(Set.empty[String]) { (acc, bagName) =>
      val bag = rules.get(bagName).get
      if (acc.contains(bagName) || bag.contains(name)) {
        acc + bagName
      } else {
        val innerBagsContainingName = bagsContaining(name, bag.innerBagNames)
        if (innerBagsContainingName.nonEmpty)
          (acc + bagName).union(innerBagsContainingName)
        else
          acc
      }
    }
  }

  def bagsInside(name: String): Int = {
    rules.get(name).get.innerBags.foldLeft(0){ case (acc, (count, innerBag)) =>
      acc + count * (1 + bagsInside(innerBag))
    }
  }

}
