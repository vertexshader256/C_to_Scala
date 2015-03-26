package tests

import org.scalatest._
import reflect.runtime.universe._

class SimpleTypedef extends FlatSpec with ShouldMatchers {

  "A typedef with 2 simple arguments" should "swap order before becoming a type alias" in {

    assert("typedef S32 LATLON;" ==> "type LATLON = S32")
  }
}