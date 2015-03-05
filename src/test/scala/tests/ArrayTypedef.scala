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

class ArrayTypedefInt extends FlatSpec with ShouldMatchers {

  "An array typedef conversion" should "convert correctly" in {

    convertedToScala("typedef U32 OBSTACLE_LIST[(2048)];").head should equal("type OBSTACLE_LIST = Array[U32]")
  }
}