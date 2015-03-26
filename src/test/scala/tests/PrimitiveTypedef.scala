package tests

import org.scalatest._

class PrimitiveTypedefInt extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    assert("typedef int LATLON;" ==> "type LATLON = Int")
  }
  
  "A simple unsigned typedef conversion" should "convert correctly" in {
    assert("typedef unsigned int LATLON;" ==> "type LATLON = Int")
  }
  
  "A simple signed typedef conversion" should "convert correctly" in {
    assert("typedef signed int LATLON;" ==> "type LATLON = Int")
  }
}

class PrimitiveTypedefShort extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    assert("typedef short LATLON;" ==> "type LATLON = Short")
  }
}

class PrimitiveTypedefFloat extends FlatSpec with ShouldMatchers {
  "A simple typedef conversion" should "convert correctly" in {
    assert("typedef float LATLON;" ==> "type LATLON = Float")
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