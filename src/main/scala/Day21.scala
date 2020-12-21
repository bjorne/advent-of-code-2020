import scala.io.Source

object Day21 extends App {
  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day21.txt")
    case _               => throw new IllegalArgumentException("Usage: Day21 [filename]")
  }

  val input = source.getLines.toList
  println(input)

  lazy val Line = """(.+) \(contains (.+)\)""".r

  private def parseAndMatch(lines: Seq[String]) = {
    val parsed = lines
      .map {
        case Line(ingrStr, allergStr) =>
          (
            ingrStr.split(" "),
            allergStr
              .split(",")
              .map(_.trim)
          )
      }
    val ingredientCount = parsed.foldLeft(Map.empty[String, Int]) {
      case (m, (ingredients, _)) =>
        countThings(m, ingredients)
    }
    println(ingredientCount)
    val allergenCount = parsed.foldLeft(Map.empty[String, Int]) {
      case (m, (_, allergens)) =>
        countThings(m, allergens)
    }
    println(allergenCount)
    val allergenIngrCount =
      parsed.foldLeft(Map.empty[String, Map[String, Int]]) {
        case (allMap, (ingredients, allergens)) =>
          allergens
            .foldLeft(allMap) { (mm, allergen) =>
              mm.updatedWith(allergen) { mma =>
                Some(
                  countThings(
                    mma.getOrElse(Map.empty[String, Int]),
                    ingredients
                  )
                )
              }
            }
      }
    println(allergenIngrCount)
    val ingredientAlwaysWithAllergen =
      allergenCount.foldLeft(Map.empty[String, Set[String]]) {
        case (map, allergen -> count) =>
          map.updated(allergen, allergenIngrCount(allergen).collect {
            case (ingr, ingrCount) if ingrCount == count => ingr
          }.toSet)
      }
    println("ingredientAlwaysWithAllergen", ingredientAlwaysWithAllergen)
    val matched =
      matchIngredients(ingredientAlwaysWithAllergen, Map.empty[String, String])
    (matched, ingredientCount)
  }
  def answer(lines: Seq[String]): Int = {
    val (matched, ingredientCount) = parseAndMatch(lines)
    ingredientCount.filterNot(ic => matched.keySet.contains(ic._1)).values.sum
  }

  def answer2(lines: Seq[String]) = {
    val (matched, _) = parseAndMatch(lines)
    matched.toList.sortBy(_._2).map(_._1).mkString(",")
  }

  def matchIngredients(allergenToIngredients: Map[String, Set[String]],
                       matches: Map[String, String]): Map[String, String] = {
    val single = allergenToIngredients.collectFirst {
      case (allergen, ingrSet) if ingrSet.size == 1 =>
        (allergen, ingrSet.head)
    }
    single match {
      case Some(allergen -> ingredient) =>
        matchIngredients(
          allergenToIngredients
            .filter(_._1 != allergen)
            .view
            .mapValues(_ - ingredient)
            .toMap,
          matches.updated(ingredient, allergen)
        )
      case None => matches
    }
  }

  private def countThings(m: Map[String, Int], ingredients: Array[String]) = {
    ingredients.foldLeft(m) { (acc, ingr) =>
      acc.updatedWith(ingr) {
        case Some(n) => Some(n + 1)
        case None    => Some(1)
      }
    }
  }

  println(answer(input))
  println(answer2(input))
}
