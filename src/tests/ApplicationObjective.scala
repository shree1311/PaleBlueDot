package tests

import org.scalatest._
import pbd.PaleBlueDot

class ApplicationObjective extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1"){
      var output = PaleBlueDot.closestCity(citiesFilename,List(78.0666667,14.2333333))
      var expected : List[String] = List("sj","barentsburg","00")
      assert(output == expected,"i got" + output)
  }

  test("test 2"){
    var output = PaleBlueDot.closestCity(citiesFilename,List(-31.95233,115.862386))
    var expected : List[String] = List("au","perth","08")
    assert(output == expected,"i got" + output)
  }

  test("test 3"){
    var output = PaleBlueDot.closestCity(citiesFilename,List(-84.9167,124.7253))
    var expected : List[String] = List("tf","port-aux-francais","00")
    assert(output == expected,"i got " + output)
  }

  test("test 4"){
    var output = PaleBlueDot.closestCity(citiesFilename,List(-90,90))
    var expected : List[String] = List("ar", "ushuaia", "23")
    assert(output == expected,"i got " + output)
  }

  test("test 5"){
    var output = PaleBlueDot.closestCity(citiesFilename,List(90,90))
    var expected : List[String] = List("sj", "ny-alesund", "00")
    assert(output == expected,"i got " + output)
  }
}
