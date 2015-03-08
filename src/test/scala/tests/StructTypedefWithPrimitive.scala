package tests

import org.scalatest._

class StructTypedefWithPrimitive extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = """typedef struct {
                    int lat ;
                    int lon ;
                  } LL;"""
    
    convertedToScala(test) should equal(Array("class LL {",
                                              "var lat: Int = 0",
                                              "var lon: Int = 0",
                                              "}"))
  }
}