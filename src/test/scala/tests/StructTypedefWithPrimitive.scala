package tests

import org.scalatest._
import com.c2scala.CParser
import com.c2scala.CLexer

import com.c2scala.CConverter
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;

class StructTypedefWithPrimitive extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef struct {\n" +
      "int lat ;\n" +
      "int lon ;\n" +
      "} LL;"
    
    convertedToScala(test) should equal(Array("class LL {",
                                              "var lat: Integer = 0",
                                              "var lon: Integer = 0",
                                              "}"))
  }
}