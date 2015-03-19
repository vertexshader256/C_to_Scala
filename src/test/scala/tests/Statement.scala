package tests

import org.scalatest._

class Statement extends FlatSpec with ShouldMatchers {

  "A simple statement" should "convert correctly" in {
    convertedToScala("int blah;").head should equal("var blah: Int = 0")
  }
  
  "A simple statement with initial value" should "convert correctly" in {
    convertedToScala("int blah = 5;").head should equal("var blah: Int = 5")
  }
    
  "A const integer with initial value" should "convert correctly" in {
    convertedToScala("const int blah = 5;").head should equal("val blah: Int = 5")
  }
  
  "A static custom typed variable" should "convert correctly" in {
    convertedToScala("static X blah;").head should equal("private var blah: X = null")
  }
  
  "A static chained custom typed variable" should "convert correctly" in {
    convertedToScala("typedef double Blah; typedef Blah Test; static Test blah;") should equal(Array("type Blah = Double", "type Test = Blah", "private var blah: Test = 0.0"))
  }
  
  "A simple statement with custom type" should "convert correctly" in {
    convertedToScala("LATLON blah;").head should equal("var blah: LATLON = null")
  }
  
  "Two variables of a custom type being simultaneously declared" should "convert correctly" in {
    convertedToScala("LATLON x, y;").head should equal("var (x: LATLON, y: LATLON) = (null, null)")
  }
   
  "A simple statement setting a floating point number" should "convert correctly" in {
    convertedToScala("float blah = 5.0;").head should equal("var blah: Float = 5.0f")
  }
  
  "A simple statement with an array" should "convert correctly" in {
    convertedToScala("X blah[Y];").head should equal("var blah: Array[X] = Array.fill(Y)(null)")
  }
  
  "Multiple simple statements" should "convert correctly" in {
    convertedToScala("X blah[Y]; X Z;") should equal(Array("var blah: Array[X] = Array.fill(Y)(null)", "var Z: X = null"))
  }
  
  "Two variables of a primitive type being simultaneously declared" should "convert correctly" in {
    convertedToScala("float x, y;").head should equal("var (x: Float, y: Float) = (0.0, 0.0)")
  }
  
  "Four variables of a primitive type being simultaneously declared" should "convert correctly" in {
    convertedToScala("float x, y, r, s;").head should equal("var (x: Float, y: Float, r: Float, s: Float) = (0.0, 0.0, 0.0, 0.0)")
  }
  
  "Two variables of a primitive type being declared diff values" should "convert correctly" in {
    convertedToScala("float x = 2.0, y = 3.0;").head should equal("var (x: Float, y: Float) = (2.0, 3.0)")
  }
  
  "Two variables of a primitive type being declared negative values" should "convert correctly" in {
    convertedToScala("float x = -2.0, y = -3.0;").head should equal("var (x: Float, y: Float) = (-2.0, -3.0)")
  }
  
   "Two statements of a primitive type being declared" should "convert correctly" in {
    convertedToScala("float x = 2.0, y = 3.0; int r = 1, s = 3;") should equal(Array("var (x: Float, y: Float) = (2.0, 3.0)",
                                                                                     "var (r: Int, s: Int) = (1, 3)"))
  }
}