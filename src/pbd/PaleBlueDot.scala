package pbd

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}


object PaleBlueDot {


  /**
   * Task 1
   *
   * Given a country name using a mix of case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename and the countryName input
   * of your method is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName in lowercase letters
   *         * "Japan" -> "jp",
   *"Belgium" -> "be",
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    val countriesFile: BufferedSource = Source.fromFile(countriesFilename)
    var code: String = ""
    for (line <- countriesFile.getLines()){
      val strip: Array[String] = line.split("#")
      if (strip(0).toLowerCase() == countryName.toLowerCase()){
        code = strip(1)
      }
    }
    code.toLowerCase()
  }


  /**
   * Task 2
   *
   * Find the average population of cities in a country
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The average population of cities in the given country
   */
  def averagePopulation(countriesFilename: String, citiesFilename: String, countryName: String): Double = {
    val countryCode : String = getCountryCode(countriesFilename, countryName)
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    var totalPopulation : Double = 0
    var totalCount: Double = 0
    for (line <- citiesFile.getLines()){
      val citiesInfo: Array[String] = line.split(",")
      if (citiesInfo(0) == countryCode){
        totalPopulation += citiesInfo(3).toDouble
        totalCount += 1.0
      }
    }
    (totalPopulation/totalCount).toDouble
  }


  /**
   * Task 3
   *
   */

  /**
   * Returns a Map[cityName -> population] for all cities in the given county. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int. The country name may contain any mix of upper/lower-case letters.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return A Map containing the name and population of every city in the given country
   */
  def cityPopulations(countriesFilename: String, citiesFilename: String, countryName: String): Map[String, Int] = {
    val countryCode : String = getCountryCode(countriesFilename, countryName)
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    var retMap = Map.empty[String, Int]
    for (line <- citiesFile.getLines()){
      val citiesInfo : Array[String] = line.split(",")
      if (citiesInfo(0) == countryCode){
        if (citiesInfo(1) != retMap){
          var cityName : String = citiesInfo(1)
          var cityPopulation : Int = citiesInfo(3).toInt
          retMap = retMap + (cityName -> cityPopulation)
        }
      }
    }
    retMap
  }


  /**
   * Returns a List of city names in the given county and with above average population for that country
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return All city names in given country with a population > the average populations of cities in that country
   */
  def aboveAverageCities(countriesFilename: String, citiesFilename: String, countryName: String): List[String] = {
    val countryCode : String = getCountryCode(countriesFilename, countryName)
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    var retVal = List[String]()
    val averagePop : Double = averagePopulation(countriesFilename,citiesFilename,countryName)
    val cities : Map [String, Int] = cityPopulations(countriesFilename,citiesFilename,countryName)
    for ((key,value) <- cities){
      if (value > averagePop){
        retVal :+= key
      }
    }
    retVal
  }


  /**
   * Application Objective
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * Return the closest city to the given location in terms of greater circle distance which is the shortest distance
   * needed to walk along the surface of the Earth to reach a city.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file (ie. the List should have exactly 3 values to return
   *         a single city
   */
  def closestCity(citiesFilename: String, location: List[Double]): List[String] = {
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    val lat1 : Double = location(0)
    val long1 : Double = location(1)
    var finDistance : Double = 100000.0
    var retVal : List[String] = List()
    for (line <- citiesFile.getLines().drop(1)){
      val citiesInfo : Array[String] = line.split(",")
      val lat2= citiesInfo(4).toDouble
      val long2 = citiesInfo(5).toDouble
      var distance : Double = distanceCalc(lat1,long1,lat2,long2)
      var cityList: List[Double] = List(citiesInfo(4).toDouble, citiesInfo(5).toDouble)
      if (distance < finDistance) {
          finDistance = distance
          retVal = List(citiesInfo(0), citiesInfo(1), citiesInfo(2))
      }
    }
   retVal
  }


  def distanceCalc(userLat : Double, userlong : Double, cityLat : Double, cityLong: Double): Double ={
    val R : Double = 6371
    val latDistance = Math.toRadians(userLat-cityLat)
    val longDistance = Math.toRadians(userlong-cityLong)
    val sinLat = Math.sin(latDistance / 2)
    val sinLong = Math.sin(longDistance / 2)
    val a = sinLat * sinLat +
      (Math.cos(Math.toRadians(userLat)) *
        Math.cos(Math.toRadians(cityLat))*
        sinLong*sinLong)
    val c = 2*Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
    val distance = (R * c).toDouble
    distance
  }


  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }


  def main(args: Array[String]): Unit = {
    openMap(List(43.002743, -78.7874136))
  }

}
