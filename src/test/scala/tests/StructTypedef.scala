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

class StructTypedef extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef struct {\n" +
      "LATLON lat ;\n" +
      "LATLON lon ;\n" +
      "} LL;"
    
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
    listener.results(0).trim.split("\n").map(_.trim) should equal(Array("class LL {",
                                                                        "var lat: LATLON = null",
                                                                        "var lon: LATLON = null",
                                                                        "}"))
  }
  
  "A typedef struct with an array" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef struct {\n" +
      "LATLON lat[2048] ;\n" +
      "LATLON lon ;\n" +
      "} LL;"
    
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
    listener.results(0).trim.split("\n").map(_.trim) should equal(Array("class LL {",
                                                                        "var lat: Array[LATLON] = Array.fill(2048)(null)",
                                                                        "var lon: LATLON = null",
                                                                        "}"))
  }
  
  "A typedef struct with an array size surrounded in parenthesis" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef struct {\n" +
      "LATLON lat[((2048))] ;\n" +
      "LATLON lon ;\n" +
      "} LL;"
    
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
    listener.results(0).trim.split("\n").map(_.trim) should equal(Array("class LL {",
                                                                        "var lat: Array[LATLON] = Array.fill(2048)(null)",
                                                                        "var lon: LATLON = null",
                                                                        "}"))
  }
}