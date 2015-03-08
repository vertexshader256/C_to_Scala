package tests

import org.scalatest._

class DualPrimitiveTypedefInt extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef unsigned int LATLON;").head should equal("type LATLON = Int")
  }
}