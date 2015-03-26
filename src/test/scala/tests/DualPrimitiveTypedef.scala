package tests

import org.scalatest._

class DualPrimitiveTypedefInt extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    assert("typedef unsigned int LATLON;" ==> "type LATLON = Int")
  }
}