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
      
      val noIncludesDir = new File("noincludes")
      if (!noIncludesDir.exists) {
        noIncludesDir.mkdir
      } else {
        noIncludesDir.listFiles().foreach(_.delete)
      }
      
      val extractedDir = new File("extracted")
      if (!extractedDir.exists) {
        extractedDir.mkdir
      } else {
        extractedDir.listFiles().foreach(_.delete)
      }
      
      val preprocessedDir = new File("preprocessed")
      if (!preprocessedDir.exists) {
        preprocessedDir.mkdir
      } else {
        preprocessedDir.listFiles().foreach(_.delete)
      }
      
      val postprocessedDir = new File("postprocessed")
      if (!postprocessedDir.exists) {
        postprocessedDir.mkdir
      } else {
        postprocessedDir.listFiles().foreach(_.delete)
      }
      
      cCodeDir.listFiles.foreach{x => println(x.getName)}
      val codeFiles = cCodeDir.listFiles()
                            .filter{file => (file.getName.contains(".c") || file.getName.contains(".h")) && !file.getName.contains("version") }
      
      // create script
      
      val runGcc = new File("rungcc.bat")
      val gccwriter = new java.io.PrintWriter(runGcc)
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        gccwriter.println("cmd /c gcc -E -P " + beforePreFile.getAbsolutePath + " > preprocessed\\" + file.getName)
      }
      gccwriter.println("exit")
      gccwriter.close
      gccwriter.flush() 
      
      val runGccNoIncludes = new File("rungcc2.bat")
      val writerNoIncludes = new java.io.PrintWriter(runGccNoIncludes)
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        writerNoIncludes.println("cmd /c gcc -DACD_IS_API_ONLY -DFIXED_WING_TAWS -E -P " + beforePreFile.getAbsolutePath + " > noincludes\\" + file.getName)
      }
      writerNoIncludes.println("exit")
      writerNoIncludes.close
      writerNoIncludes.flush()  

      val includeFileMap = scala.collection.mutable.Map[String, List[String]]()
      
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        val pw = new java.io.PrintWriter(beforePreFile)
        try {
          val includeFiles = ListBuffer[String]()
          for (line <- Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines()) {
            if (!line.contains("#include")) {
              pw.println(line)
            } else if (line.split("\"").size == 2){
              includeFiles += line.split("\"")(1).reverse.drop(2).reverse
            }
          } 
          
          if (file.getName.endsWith(".c")) {
            includeFileMap(file.getName.split('.')(0)) = includeFiles.toList
          }
            
        } finally {
            pw.close
        }
      }
 
      val rt = Runtime.getRuntime();
      rt.exec("cmd /c start /wait " + runGccNoIncludes.getAbsolutePath).waitFor();
      
      val noIncludes = noIncludesDir.listFiles
      val linecountsNoIncludes = noIncludes map{file => file.getName -> Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines().size} toMap
      
      beforePreprocessingDir.listFiles.foreach(_.delete)
      noIncludes.foreach{_.delete()}
      
      
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        val pw = new java.io.PrintWriter(beforePreFile)
        try {
          for (line <- Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines()) {
              pw.println(line)
          } 
        } finally {
            pw.close
        }
        
      }
      
      rt.exec("cmd /c start /wait " + runGcc.getAbsolutePath).waitFor();
      
      beforePreprocessingDir.listFiles.foreach(_.delete)
      
      val cTypes = HashMap[String, String]()
      
      for (file <- preprocessedDir.listFiles) {


            val parser = new CParser(
            new CommonTokenStream(
            new CLexer(new ANTLRFileStream(file.getAbsolutePath))));
  
            parser.setBuildParseTree(true);
 
            
            // This line prints the error
            val ctx = parser.compilationUnit();
            val visitor = new DeclarationConverter(cTypes, false);
            visitor.visit(ctx)
      }
      
      println("WHOA: " + cTypes.size)
      cTypes.foreach(println)
      
      preprocessedDir.listFiles.foreach(_.delete)
      
      // now write the whole file (including #includes)
            
      for (file <- codeFiles) {
        val beforePreFile = new File(beforePreprocessingDir.getAbsolutePath + "\\" + file.getName)
        val pw = new java.io.PrintWriter(beforePreFile)
        try {
          for (line <- Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines()) {
            if (line.contains("#include")) {
              pw.println("==== BEGIN " + line.split("#include")(1) + " ====")
              pw.println(line)
              pw.println("==== END " + line.split("#include")(1) + " ====")
            } else {
              pw.println(line)
            }
          } 
        } finally {
            pw.close
        }
        
      }
      
      val groupings = Map("ac_data" -> Seq("ac_data.c", "ac_data.h"),
                          "ang_util" -> Seq("ang_util.c", "ang_util.h"),
                          "avg_util" -> Seq("avg_util.c", "avg_util.h"),
                          "latlon_util" -> Seq("latlon_util.c", "latlon_util.h"),
                          "ta_alert" -> Seq("ta_alert.c", "ta_alert.h"),
                          "ta_gpws" -> Seq("ta_gpws.c"),
                          "ta_line" -> Seq("ta_line.c", "ta_line.h"),
                          "ta_lowalt" -> Seq("ta_lowalt.c"),
                          "ta_main" -> Seq("ta_main.c", "ta_main.h"),
                          "ta_obstacle" -> Seq("ta_obstacle.c"),
                          "ta_parameters" -> Seq("ta_parameters.c", "ta_parameters.h"),
                          "ta_pda" -> Seq("ta_pda.c"),
                          "ta_pof" -> Seq("ta_pof.c"),
                          "ta_pred" -> Seq("ta_pred.c"),
                          "ta_util" -> Seq("ta_util.c", "ta_util.h"),
                          "ta_vap" -> Seq("ta_vap.c"),
                          "taws_api" -> Seq("taws_api.c", "taws_api.h"),
                          "taws_callouts" -> Seq("taws_callouts.c"),
                          "terr_util" -> Seq("terr_util.c", "terr_util.h"),
                          "trig" -> Seq("trig.c", "trig.h"),
                          "api_common" -> Seq("api_common.h"),
                          "api_obst_defs" -> Seq("api_obst_defs.h"),
                          "api_terr_defs" -> Seq("api_terr_defs.h"),
                          "api_a424_defs" -> Seq("api_a424_defs.h"),
                          "ta_defs" -> Seq("ta_defs.h")
                          )
      
      rt.exec("cmd /c start /wait " + runGcc.getAbsolutePath).waitFor();
      
      //beforePreprocessingDir.listFiles.foreach(_.delete)
      
      val fullPreprocess = preprocessedDir.listFiles
      
      //extract only code that was in the original file
      
      for ((name, components) <- groupings) {
         for (file <- fullPreprocess) {
  
            if (file.getName == components.head) {
              val extractedData = new File(extractedDir.getAbsolutePath + "\\" + name)
              val pw = new java.io.PrintWriter(extractedData)
              val expandedLines = Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines().toList
              var level = 0
              var level2 = 0
              var isWithinHeaderInclude = false
            
              for (line <- expandedLines) {
                if (line.contains("==== BEGIN ")) {
                  level2 += 1
                  if (components.tail.exists{ x => line.contains(x)}) {
                    isWithinHeaderInclude = true
                  } else if (isWithinHeaderInclude) {
                    level += 1
                  }
                } else if (line.contains("==== END ")) {
                  level2 -= 1
                  if (components.tail.exists{ x => line.contains(x)}) {
                    isWithinHeaderInclude = false
                  } else if (isWithinHeaderInclude) {
                    level -= 1
                  }
                } else if (level2 == 0 || (isWithinHeaderInclude && level == 0)) {
                  pw.println(line)
                }
              }
              pw.close()
            }
         }
      }
  
      runGcc.delete
      
      for (file <- extractedDir.listFiles) { 
        //val name = file.getName.reverse.drop(2).reverse
        val postFile = new File(postprocessedDir.getAbsolutePath + "\\" + file.getName)
        val pw = new java.io.PrintWriter(postFile)
        try {
            val lines = Source.fromFile(file.getAbsolutePath, "ISO-8859-1").getLines()
            for (line <- lines) {
              pw.println(line)
            }
        } finally {
            pw.close
        }
        
        val parser = new CParser(
            new CommonTokenStream(
            new CLexer(new ANTLRFileStream("postprocessed\\" + file.getName))));
  
        parser.setBuildParseTree(true);
  
        //val cTypes = HashMap[String, String]()
        
        // This line prints the error
        val ctx = parser.compilationUnit();
        val visitor = new DeclarationConverter(cTypes, false);
        visitor.visit(ctx)

        println("RESULTS: ")
        
        if (visitor.results.size > 0) {
          
          val resultWriter = new PrintWriter(new FileOutputStream("convertedCode\\" + file.getName + ".scala"))
          
          resultWriter.println("package convertedCode\n\n")
          includeFileMap.withDefaultValue(Nil)(file.getName).foreach{x => resultWriter.println("import " + x + "._")}
          resultWriter.println("object " + file.getName + " {\n")
          visitor.results.foreach{line => resultWriter.println(line)}
          resultWriter.println("}\n")
          
          resultWriter.flush
          resultWriter.close
        }
      }
    }   
}