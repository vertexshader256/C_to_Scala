package com.c2scala

import java.util.Arrays;
import javax.swing.JFrame;
import javax.swing.JPanel;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.gui.TreeViewer;
import scala.collection.JavaConverters._

object visualize {

  def main(arg: Array[String]) = {     
    val parser = new CParser(
              new CommonTokenStream(
                      new CLexer(
                              new ANTLRInputStream("int blah(long x) {x += y;}"))));
    
      parser.setBuildParseTree(true);

      // This line prints the error   
      val frame = new JFrame("Antlr AST");
        val panel = new JPanel();
        val viewr = new TreeViewer(parser.getRuleNames().toList.asJava,parser.compilationUnit());
        viewr.setScale(1.5);//scale a little
        panel.add(viewr);
        frame.add(panel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(200,200);
        frame.setVisible(true);
        println("DONE!")
  }
  
}