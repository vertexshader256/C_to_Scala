package tests

import org.scalatest._
import reflect.runtime.universe._

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
    assert(convertedToScalaTree("typedef long LATLON;") equalsStructure q"type LATLON = Long")
  }
  
  "A simple unsigned typedef conversion" should "convert correctly" in {
    assert(convertedToScalaTree("typedef unsigned long LATLON;") equalsStructure q"type LATLON = Long")
  }
  
  "A simple signed typedef conversion" should "convert correctly" in {
    assert(convertedToScalaTree("typedef signed long LATLON;") equalsStructure q"type LATLON = Long")
  }
}