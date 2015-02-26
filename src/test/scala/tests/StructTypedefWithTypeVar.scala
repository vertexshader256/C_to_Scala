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
    
    val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRInputStream(test))));
  
    parser.setBuildParseTree(true);

    // This line prints the error
    val ctx = parser.compilationUnit();
    val listener = new CConverter();
    ParseTreeWalker.DEFAULT.walk(listener, ctx); 

    println("here: " + listener.results(0).trim)
    listener.results(0).trim.split("\n").map(_.trim) should equal(Array("case class POINT_OBSTACLE(",
                                                                        "var obstacle_type: OBSTACLE_TYPE",
                                                                        ")"))
  }
}