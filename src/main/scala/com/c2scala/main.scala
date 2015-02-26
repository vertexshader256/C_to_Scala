package com.c2scala

import java.io.IOException;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;
import java.util._
import java.io._
import scala.io.Source

object main {
  
    def writeToFile(file: String, contents: String): Unit = {
      val pw = new java.io.PrintWriter(new File(file))
      try pw.write(contents) finally pw.close()
    }
  
    def main(arg: Array[String]) = {        
      
      val cCodeDir = new File("C:\\Scala\\sandel_baseline\\src")
      val convertedCodeDir = new File("convertedCode")
      if (!convertedCodeDir.exists) {
        convertedCodeDir.mkdir
      } else {
        convertedCodeDir.listFiles().foreach(_.delete)
      }
      
      val beforePreprocessingDir = new File("beforePre")
      if (!beforePreprocessingDir.exists) {
        beforePreprocessingDir.mkdir
      } else {
        beforePreprocessingDir.listFiles().foreach(_.delete)
      }
      
      for (cCodeFile <- cCodeDir.listFiles().filter(_.getName.endsWith(".h"))) {
        val fileName = cCodeFile.getName
        val fileNameWithoutExtension = cCodeFile.getName.substring(0, cCodeFile.getName.lastIndexOf('.'))
        
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + fileName)
        val pw = new java.io.PrintWriter(beforePreFile)
        println(beforePreFile.getAbsolutePath)
        try {
          for (line <- Source.fromFile(cCodeFile.getAbsolutePath, "ISO-8859-1").getLines()) {
            if (!line.contains("#include")) {
              pw.println(line)
            }
          } 
        } finally {
          pw.close
        }
        
        val rt = Runtime.getRuntime();
        val proc = rt.exec("cmd /c gcc -E -P " + beforePreFile.getAbsolutePath + " > preprocessed_" + fileName)
        
        // any error???
        val exitVal = proc.waitFor();
        println("ExitValue: " + exitVal);
  
        val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRFileStream("preprocessed_" + fileName))));
  
        parser.setBuildParseTree(true);
  
        // This line prints the error
        val ctx = parser.compilationUnit();
        val listener = new CConverter();
        ParseTreeWalker.DEFAULT.walk(listener, ctx); 
  
        println("RESULTS: ")
        
        val resultWriter = new PrintWriter(new FileOutputStream("convertedCode\\" + fileNameWithoutExtension + ".scala"))
        
        resultWriter.println("package convertedCode\n\n")
        resultWriter.println("object " + fileNameWithoutExtension + " {\n")
        listener.results.foreach{line => resultWriter.println(line)}
        resultWriter.println("}\n")
        
        resultWriter.flush
        resultWriter.close
        
        val preprocessedFile = new File("preprocessed_" + fileName)
        //preprocessedFile.delete()
      }
    }   
}