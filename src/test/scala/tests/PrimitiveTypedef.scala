package tests

import org.scalatest._

class PrimitiveTypedefInt extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef int LATLON;").head should equal("type LATLON = Int")
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

    convertedToScala("typedef long LATLON;").head should equal("type LATLON = Long")
  }
}