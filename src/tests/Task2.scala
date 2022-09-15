package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"
  val epsilon : Double = 0.01
  test("test 1") {
    val output = PaleBlueDot.averagePopulation(countriesFile, citiesFilename,"United Arab Emirates")
    val expected: Double = 761668.33
    assert(Math.abs( output - expected) < epsilon)
  }

  test("test 2"){
    val output = PaleBlueDot.averagePopulation(countriesFile,citiesFilename,"Anguilla")
    val expected = 1379
    assert(Math.abs(output - expected) < epsilon)

  }

  test("test 3"){
    val output = PaleBlueDot.averagePopulation(countriesFile,citiesFilename,"anguilla")
    val expected = 1379
    assert(Math.abs(output-expected)<epsilon)
  }
}

