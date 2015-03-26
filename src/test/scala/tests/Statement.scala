package tests

import org.scalatest._

class Statement extends FlatSpec with ShouldMatchers {

  "A simple statement" should "convert correctly" in {
    assert("int blah;" ==> "var blah: Int = 0")
  }
  
  "A simple statement with initial value" should "convert correctly" in {
    assert("int blah = 5;" ==> "var blah: Int = 5")
  }
    
  "A const integer with initial value" should "convert correctly" in {
    assert("const int blah = 5;" ==> "val blah: Int = 5")
  }
  
  "A static custom typed variable" should "convert correctly" in {
    assert("static X blah;" ==> "private var blah: X = null")
  }
  
  "A static chained custom typed variable" should "convert correctly" in {
    assert("typedef double Blah; typedef Blah Test; static Test blah;" ==> "type Blah = Double; type Test = Blah; private var blah: Test = 0.0")
  }
  
  "A simple statement with custom type" should "convert correctly" in {
    assert("LATLON blah;" ==> "var blah: LATLON = null")
  }
  
  "Two variables of a custom type being simultaneously declared" should "convert correctly" in {
    assert("LATLON x, y;" ==> "var (x: LATLON, y: LATLON) = (null, null)")
  }
   
  "A simple statement setting a floating point number" should "convert correctly" in {
    assert("float blah = 5.0;" ==> "var blah: Float = 5.0f")
  }
  
  "A simple statement with an array" should "convert correctly" in {
    assert("X blah[Y];" ==> "var blah: Array[X] = Array.fill(Y)(null)")
  }
  
  "A simple statement with an 2d array" should "convert correctly" in {
    assert("int blah[1][2];" ==> "var blah: Array[Array[Int]] = Array.fill(1)(Array.fill(2)(0))")
  }
  
  "A 2d array with an array initializer" should "be converted to nested arrays" in {
    assert("int blah[2][2] = {{1,2},{3,4}};" ==> "var blah: Array[Array[Int]] = Array(Array(1,2),Array(3,4))")
  }
  
  "A 1d array with an array initializer" should "be converted to an array" in {
    assert("int blah[2] = {1,2};" ==> "var blah: Array[Int] = Array(1,2)")
  }
  
  "Multiple simple statements" should "convert correctly" in {
    assert("X blah[Y]; X Z;" ==> "var blah: Array[X] = Array.fill(Y)(null); var Z: X = null")
  }
  
  "Two variables of a primitive type being simultaneously declared" should "convert correctly" in {
    assert("float x, y;" ==> "var (x: Float, y: Float) = (0.0f, 0.0f)")
  }
  
  "Four variables of primitive float type being simultaneously declared" should "be all set to initial floating point defaults" in {
    assert("float x, y, r, s;" ==> "var (x: Float, y: Float, r: Float, s: Float) = (0.0f, 0.0f, 0.0f, 0.0f)")
  }
  
  "Two variables of a primitive type being declared diff values" should "convert correctly" in {
    assert("float x = 2.0, y = 3.0;" ==> "var (x: Float, y: Float) = (2.0f, 3.0f)")
  }
  
  "Two variables of a primitive type being declared negative values" should "convert correctly" in {
    assert("float x = -2.0, y = -3.0;" ==> "var (x: Float, y: Float) = (-2.0f, -3.0f)")
  }
  
   "Two statements of a primitive type being declared" should "convert correctly" in {
    assert("float x = 2.0, y = 3.0; int r = 1, s = 3;" ==> """var (x: Float, y: Float) = (2.0f, 3.0f);
                                                              var (r: Int, s: Int) = (1, 3)""")
  }
}