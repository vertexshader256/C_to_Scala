package tests

import org.scalatest._

class ArrayTypedefInt extends FlatSpec with ShouldMatchers {

  "An array typedef conversion" should "convert correctly" in {

    convertedToScala("typedef U32 OBSTACLE_LIST[(2048)];").head should equal("type OBSTACLE_LIST = Array[U32]")
  }
}