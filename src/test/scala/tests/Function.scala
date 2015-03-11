package tests

import org.scalatest._

class Function extends FlatSpec with ShouldMatchers {

  "A simple function" should "convert correctly" in {

    convertedToScala("int blah() {}").head should equal("def blah(): Int = {}")
  }
  
  "A simple function with a primitive parameter" should "convert correctly" in {

    convertedToScala("float blah(int x) {}").head should equal("def blah(x: Int): Float = {}")
  }
  
  "A simple function with a multiple primitive parameters" should "convert correctly" in {

    convertedToScala("float blah(int x, int y) {}").head should equal("def blah(x: Int, y: Int): Float = {}")
  }
  
  "A simple function with a custom parameter" should "convert correctly" in {

    convertedToScala("float blah(LATLON x) {}").head should equal("def blah(x: LATLON): Float = {}")
  }
  
  "A simple function with a custom return type" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {}").head should equal("def blah(x: Long): LAT = {}")
  }
  
  "Two simple functions" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {}; float blah(LATLON x) {};") should equal(Array("def blah(x: Long): LAT = {}",
                                                                                         "def blah(x: LATLON): Float = {}"))
  }
  
  "Two simple functions with simple contents" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {int i;}; float blah(LATLON x) {int j;};") should equal(Array("def blah(x: Long): LAT = {var i: Int = 0}",
                                                                                                     "def blah(x: LATLON): Float = {var j: Int = 0}"))
  }
  
  "A simple function with contents" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {int i;}").head should equal("def blah(x: Long): LAT = {var i: Int = 0}")
  }
  
  "A simple function with custom contents" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {LATLON i;}").head should equal("def blah(x: Long): LAT = {var i: LATLON = null}")
  }
  
  "A simple function with an assignment operator" should "convert correctly" in {
    convertedToScala("int blah(long x) {x += y;}").head should equal("def blah(x: Long): Int = {x += y}")
  }
  
  "A simple function with an two assignment operators" should "convert correctly" in {
    convertedToScala("int blah(long x) {x += y; x += z;}").head should equal("def blah(x: Long): Int = {x += y; x += z}")
  }
  
  "A simple function with a complex assignment" should "convert correctly" in {
    convertedToScala("int blah(long x) {x = 1 + 2 + 3;}").head should equal("def blah(x: Long): Int = {x = 1 + 2 + 3}")
  }
  
  "A simple function with a more complex assignment" should "convert correctly" in {
    convertedToScala("int blah(long x) {x = 1 - 2 + 3 / 4 * 5;}").head should equal("def blah(x: Long): Int = {x = 1 - 2 + 3 / 4 * 5}")
  }
  
  "A simple function with a more complex assignment with a variable ref" should "convert correctly" in {
    convertedToScala("int blah(long x) {x = z - 2 + y / 4 * u;}").head should equal("def blah(x: Long): Int = {x = z - 2 + y / 4 * u}")
  }
  
//  "A simple function with a more complex assignment with a struct ref" should "convert correctly" in {
//    convertedToScala("int blah(long x) {x = z - 2 + y.x / 4 * u;}").head should equal("def blah(x: Long): Int = {x = z - 2 + y.x / 4 * u}")
//  }
}