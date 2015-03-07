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

class PrimitiveTypedefInt extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef int LATLON;").head should equal("type LATLON = Int")
  }
}

class PrimitiveTypedefShort extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef short LATLON;").head should equal("type LATLON = Short")
  }
}

class PrimitiveTypedefFloat extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef float LATLON;").head should equal("type LATLON = Float")
  }
}

class PrimitiveTypedefLong extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("typedef long LATLON;").head should equal("type LATLON = Long")
  }
}