package com.c2scala

import java.io.IOException;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;
import scala.collection.mutable.ListBuffer
import java.util._
import java.io._
import scala.io.Source
import scala.collection.mutable.HashMap
import java.nio.file.Files

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
      cCodeDir.listFiles.foreach{x => println(x.getName)}
      val codeFiles = cCodeDir.listFiles()
                            .filter{file => (file.getName.contains(".c") || file.getName.contains(".h")) && !file.getName.contains("version") }
                            //.groupBy{file => file.getName.split('.')(0)}

      // we're doing the most basic preprocessing - grouping the .c and .h together
      
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        val newCopy = new File(file.getName)
        Files.copy( file.toPath, beforePreFile.toPath );
      }
      
      val runGcc = new File("rungcc.bat")
      val gccwriter = new java.io.PrintWriter(runGcc)
      val includeFiles = new ListBuffer[String]()
      
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        val pw = new java.io.PrintWriter(beforePreFile)
         
        try {
            for (line <- Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines()) {
              //if (!line.contains("#include")) {
                pw.println(line)
              if (line.contains("\"")){
                // extract file name, substract .h extension
                includeFiles += line.split("\"")(1).reverse.drop(2).reverse
              }
            } 
        } finally {
            pw.close
        }
        //println("cmd /c gcc -E -P " + beforePreFile.getAbsolutePath + " > preprocessed_" + file.getName)
        
       // val newCopy = new File(beforePreFile.getName)
        //Files.copy( beforePreFile.toPath, newCopy.toPath );
        
        
        gccwriter.println("cmd /c gcc -E -P " + beforePreFile.getAbsolutePath + " > preprocessed_" + file.getName)
      }
      gccwriter.println("exit")
        val rt = Runtime.getRuntime();
        gccwriter.close
        gccwriter.flush()
        val proc = rt.exec("cmd /c start /wait " + runGcc.getAbsolutePath)
        //runGcc.delete
        //newCopy.delete()
        // any error???
        val exitVal = proc.waitFor();
        runGcc.delete
        
      for (file <- codeFiles) { 
        val newFile = new File("preprocessed_" + file.getName)
        do {
          
        } while (!newFile.exists());

  
        val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRFileStream("preprocessed_" + file.getName))));
  
        parser.setBuildParseTree(true);
  
         val cTypes = HashMap[String, String]()
        
        // This line prints the error
        val ctx = parser.compilationUnit();
        val visitor = new DeclarationConverter(cTypes, false);
        visitor.visit(ctx)

        println("RESULTS: ")
        
        if (visitor.results.size > 0) {
          
          val resultWriter = new PrintWriter(new FileOutputStream("convertedCode\\" + file.getName + ".scala"))
          
          resultWriter.println("package convertedCode\n\n")
          includeFiles.foreach{x => resultWriter.println("import " + x + "._")}
          resultWriter.println("object " + file.getName + " {\n")
          visitor.results.foreach{line => resultWriter.println(line)}
          resultWriter.println("}\n")
          
          resultWriter.flush
          resultWriter.close
          
          val preprocessedFile = new File("preprocessed_" + file.getName)
          //preprocessedFile.delete()
        }
      }
    }   
}