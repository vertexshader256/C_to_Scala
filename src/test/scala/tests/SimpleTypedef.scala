package tests

import org.scalatest._
import reflect.runtime.universe._

class SimpleTypedef extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    assert(convertedToScalaTree("typedef S32 LATLON;") equalsStructure q"type LATLON = S32")
  }
}