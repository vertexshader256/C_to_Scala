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

class StructTypedefWithTypeVar extends FlatSpec with ShouldMatchers {

  "A simple struct typedef conversion with a variable named 'type'" should "convert correctly" in {
    val name = System.nanoTime
    
  val test = "typedef struct {\n" +
    "OBSTACLE_TYPE type;\n" +
    "} POINT_OBSTACLE;"
    
    convertedToScala(test) should equal(Array("class POINT_OBSTACLE {",
                                              "var obstacle_type: OBSTACLE_TYPE = null",
                                              "}"))
  }
}