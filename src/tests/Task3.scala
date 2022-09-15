package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1") {
    var expected : Map[String, Int] = Map("the valley" -> 1379)
    val output = PaleBlueDot.cityPopulations(countriesFile,citiesFilename,"AnGUIlla")
    assert (output == expected)
  }

  test ("test 2"){
    var expected : List[String] = List("dubai")
    val output = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"UniTed AraB EmIRAtes")
    assert (output == expected)
  }

  test ("test 3"){
    var expected : List[String] = List("san ignacio","belize").sorted
    val output = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"BeliZe").sorted
    assert (output == expected)
  }

  test ("test 4"){
    var expected: List[String] = List()
    val output = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"anGuiLla")
    assert(output == expected)
  }

  test("test 5"){
    var expected : List[String] = List()
    val output = PaleBlueDot.aboveAverageCities(countriesFile,citiesFilename,"French Southern Territories")
    assert(output == expected)
  }

}
