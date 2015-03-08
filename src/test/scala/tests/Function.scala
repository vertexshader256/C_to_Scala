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

class Function extends FlatSpec with ShouldMatchers {

  "A simple function" should "convert correctly" in {

    convertedToScala("int blah() {}").head should equal("def blah(): Int = {}")
  }
  
  "A simple function with a primitive parameter" should "convert correctly" in {

    convertedToScala("float blah(int x) {}").head should equal("def blah(x: Int): Float = {}")
  }
  
  "A simple function with a multiple primitive parameters" should "convert correctly" in {

    convertedToScala("float blah(int x, int y) {}").head should equal("def blah(x: Int, y: Int): Float = {}")
  }
  
  "A simple function with a custom parameter" should "convert correctly" in {

    convertedToScala("float blah(LATLON x) {}").head should equal("def blah(x: LATLON): Float = {}")
  }
  
  "A simple function with a custom return type" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {}").head should equal("def blah(x: Long): LAT = {}")
  }
  
  "A simple function with contents" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {int i;}").head should equal("def blah(x: Long): LAT = {var i: Int = 0}")
  }
  
  "A simple function with custom contents" should "convert correctly" in {

    convertedToScala("LAT blah(long x) {LATLON i;}").head should equal("def blah(x: Long): LAT = {var i: LATLON = null}")
  }
}