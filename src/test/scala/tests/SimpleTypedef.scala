package tests

import org.scalatest._
import reflect.runtime.universe._

class SimpleTypedef extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    assert("typedef S32 LATLON;" ==> "type LATLON = S32")
  }
}