package tests

import org.scalatest._

class SimpleTypedef extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef S32 LATLON;").head should equal("type LATLON = S32")
  }
}