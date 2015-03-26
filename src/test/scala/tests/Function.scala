package tests

import org.scalatest._

class Function extends FlatSpec with ShouldMatchers {

  "A simple function" should "convert correctly" in {
    assert("int blah() {}" ==> "def blah(): Int = {}")
  }
  
  "A simple function with void parameters" should "convert correctly" in {
    assert("float blah(void) {}" ==> "def blah(): Float = {}")
  }
  
  "A simple function with a primitive parameter" should "convert correctly" in {
    assert("float blah(int x) {}" ==> "def blah(x: Int): Float = {}")
  }
  
  "A simple function with a double parameter" should "convert correctly" in {
    assert("float blah(double x) {}" ==> "def blah(x: Double): Float = {}")
  }
  
  "A simple function with a return value" should "convert correctly" in {
    assert("float blah(int x) {return x;}" ==> "def blah(x: Int): Float = {x}")
  }
  
  "A simple function with a void return value" should "convert correctly" in {
    assert("void blah(int x) {}" ==> "def blah(x: Int): Unit = {}")
  }
  
   "A simple function with a void pointer param type" should "convert correctly" in {
    assert("void blah(void* x) {}" ==> "def blah(x: Object): Unit = {}")
  }
  
  "A simple function with a pointer custom type param" should "convert correctly" in {
    assert("float blah(custom *x) {}" ==> "def blah(x: custom): Float = {}")
  }
  
  "A function prototype" should "not output" in {
    convertedToScala("float blah(int x);").size should equal(0)
  }
  
  "A simple function with a multiple primitive parameters" should "convert correctly" in {
    assert("float blah(int x, int y) {}" ==> "def blah(x: Int, y: Int): Float = {}")
  }
  
  "A simple function with a custom parameter" should "convert correctly" in {
    assert("float blah(LATLON x) {}" ==> "def blah(x: LATLON): Float = {}")
  }
  
  "A simple function with a custom return type" should "convert correctly" in {
    assert("LAT blah(long x) {}" ==> "def blah(x: Long): LAT = {}")
  }
  
  "Two simple functions" should "convert correctly" in {
    assert("LAT blah(long x) {}; float blah(LATLON x) {};" ==> "def blah(x: Long): LAT = {}; def blah(x: LATLON): Float = {}")
  }
  
  "Two simple functions with simple contents" should "convert correctly" in {
    assert("LAT blah(long x) {int i;}; float blah(LATLON x) {int j;};" ==> "def blah(x: Long): LAT = {var i: Int = 0}; def blah(x: LATLON): Float = {var j: Int = 0}")
  }
  
  "A simple function with contents" should "convert correctly" in {
    assert("LAT blah(long x) {int i;}" ==> "def blah(x: Long): LAT = {var i: Int = 0}")
  }
  
  "A simple function with custom contents" should "convert correctly" in {
    assert("LAT blah(long x) {LATLON i;}" ==> "def blah(x: Long): LAT = {var i: LATLON = null}")
  }
  
  "A simple function with double contents" should "convert correctly" in {
    assert("LAT blah(long x) {double i;}" ==> "def blah(x: Long): LAT = {var i: Double = 0.0}")
  }
  
  "A simple function with resolved custom contents" should "convert correctly" in {
    assert("typedef double LATLON; LAT blah(long x) {LATLON i;}" ==> "type LATLON = Double; def blah(x: Long): LAT = {var i: LATLON = 0.0}")
  }
  
  
  
  "A simple function with an two assignment operators" should "convert correctly" in {
    assert("int blah(long x) {x += y; x += z;}" ==> "def blah(x: Long): Int = {x += y; x += z}")
  }
  
  "A simple function with a complex assignment" should "convert correctly" in {
    assert("int blah(long x) {x = 1 + 2 + 3;}" ==> "def blah(x: Long): Int = {x = 1 + 2 + 3}")
  }
  
  "A simple function with a more complex assignment" should "convert correctly" in {
    assert("int blah(long x) {x = 1 - 2 + 3 / 4 * 5;}" ==> "def blah(x: Long): Int = {x = 1 - 2 + 3 / 4 * 5}")
  }
  
  "A simple function with a more complex assignment with a variable ref" should "convert correctly" in {
    assert("int blah(long x) {x = z - 2 + y / 4 * u;}" ==> "def blah(x: Long): Int = {x = z - 2 + y / 4 * u}")
  }
  
  "A simple function with a struct assignment" should "convert correctly" in {
    assert("int blah() {x.test = 2;}" ==> "def blah(): Int = {x.test = 2}")
  }
  
  "A simple function with a more complex assignment with a struct ref" should "convert correctly" in {
    assert("int blah(long x) {x = z - 2 + y.x / 4 * u;}" ==> "def blah(x: Long): Int = {x = z - 2 + y.x / 4 * u}")
  }
  
  "A simple function with a more complex assignment with a pointer struct ref" should "convert correctly" in {
    assert("int blah(long x) {x = z - 2 + y->x / 4 * u;}" ==> "def blah(x: Long): Int = {x = z - 2 + y.x / 4 * u}")
  }
  
  "A simple function with parenthesis" should "convert correctly" in {
    assert("int blah(long x) {x = (z - 2) + (y / 4) * u;}" ==> "def blah(x: Long): Int = {x = (z - 2) + (y / 4) * u}")
  }
  
  "A simple function with nested parenthesis" should "convert correctly" in {
    assert("int blah(long x) {x = ((z - 2) + (y / 4)) * u;}" ==> "def blah(x: Long): Int = {x = ((z - 2) + (y / 4)) * u}")
  }
  
  "A simple function with a greater than" should "convert correctly" in {
    assert("int blah() {test = 2 > 4;}" ==> "def blah(): Int = {test = 2 > 4}")
  }
  
  "A simple function with a complex greater than" should "convert correctly" in {
    assert("int blah() {test = 2 > (-X*90);}" ==> "def blah(): Int = {test = 2 > (-X * 90)}")
  }
  
  "A simple equality statement" should "convert correctly" in {
    assert("int blah() {x = 1 == 1;}" ==> "def blah(): Int = {x = 1 == 1}")
  }
  
  "A simple IF statement" should "convert correctly" in {
    assert("int blah() {if (1 == 1) x = 2; else x = 3;}" ==> "def blah(): Int = {if (1 == 1) x = 2 else x = 3}")
  }
  
  "A more complex IF statement" should "convert correctly" in {
    assert("int blah() {if ((1 == 1) && (2 > 1)) x = 2; else x = 3;}" ==> "def blah(): Int = {if ((1 == 1) && (2 > 1)) x = 2 else x = 3}")
  }
  
  "boolean expressons" should "convert correctly" in {
    
    val operators = List("==", "&&", "&", "||", "|", ">", "<", ">=", "<=", ">>", "<<")
    
    for (op <- operators) {
      val test = "int blah() {if (1 " + op + " 1);}"
      val result = "def blah(): Int = {if (1 " + op + " 1)}"
      convertedToScala(test).head should equal(result)
    }
    
    for (op <- operators) {
      val test = "int x = 1 " + op + " 1;"
      val result = "var x: Int = 1 " + op + " 1"
      convertedToScala(test).head should equal(result)
    }
  }
  
  "assignment operators" should "convert correctly" in {
    
    val operators = List("=", "/=", "%=", "<<=", "*=", ">>=", "^=", "+=", "-=", "&=", "|=")
    
    for (op <- operators) {
      val test = "int blah() {x " + op + " 1;}"
      val result = "def blah(): Int = {x " + op + " 1}"
      convertedToScala(test).head should equal(result)
    }
  }
  
  
  "A simple function with an assignment operator" should "convert correctly" in {
    convertedToScala("int blah(long x) {x += y;}").head should equal("def blah(x: Long): Int = {x += y}")
  }
}