package tests

import org.scalatest._

class PrimitiveTypedefInt extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    convertedToScala("typedef int LATLON;").head should equal("type LATLON = Int")
  }
  
  "A simple unsigned typedef conversion" should "convert correctly" in {
    convertedToScala("typedef unsigned int LATLON;").head should equal("type LATLON = Int")
  }
  
  "A simple signed typedef conversion" should "convert correctly" in {
    convertedToScala("typedef signed int LATLON;").head should equal("type LATLON = Int")
  }
}

class PrimitiveTypedefShort extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    convertedToScala("typedef short LATLON;").head should equal("type LATLON = Short")
  }
}

class PrimitiveTypedefFloat extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    convertedToScala("typedef float LATLON;").head should equal("type LATLON = Float")
  }
}

class PrimitiveTypedefLong extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    assert("typedef long LATLON;" ==> "type LATLON = Long")
  }
  
  "A simple unsigned typedef conversion" should "convert correctly" in {
    assert("typedef unsigned long LATLON;" ==> "type LATLON = Long")
  }
  
  "A simple signed typedef conversion" should "convert correctly" in {
    assert("typedef signed long LATLON;" ==> "type LATLON = Long")
  }
}