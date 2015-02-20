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

class MyListener extends CBaseListener {
  var isWithinStruct = false
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  var specifierQualifierLevel = 0
  var currentTypeName = ""
  val results = ListBuffer[String]()
  var latestStorageSpecifier = ""
  
  override def enterSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
  }
  
  override def exitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel -= 1
  }
  
  override def enterStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    
  }
  
  override def exitStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    //isWithinStruct = false
  }
  
  override def enterTypedefName(ctx: CParser.TypedefNameContext) = {
    
  }
  
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (isWithinStruct) {
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.Identifier().getText
      } else if (specifierQualifierLevel == 2) {
        println(ctx.Identifier().getText)
        structDeclarations += "var " + ctx.Identifier().getText + ": " + currentTypeName
        println(ctx.Identifier().getText + ": " + currentTypeName)
      }
    } else if (declarationHasStruct && !isWithinStruct) {
      var result = "case class " + ctx.Identifier() + "(\n"
      //structDeclarations.foreach(println)
      if (!structDeclarations.isEmpty) {
        result += structDeclarations.map("  " + _).reduce{_ + ",\n" + _}
      }
      result += "\n)"
      results += result
    }
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("ENTER TYPE ")
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("EXIT TYPE ")
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    //println("ENTER DECLARATION ")
    latestStorageSpecifier = ""
    declarationHasStruct = false
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    declarationHasStruct = false
    //println("EXIT DECLARATION " )
  }
  
  override def enterStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    //println("ENTERING STRUCT ")
    isWithinStruct = true
    declarationHasStruct = true
    structDeclarations.clear
  }
  
  override def exitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
   // println("LEAVING STRUCT ")
    isWithinStruct = false
    var structResult = ""
    
  }
  
  override def enterStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
  }
  
  override def exitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("LEAVING TYPEDEF ")
  }
}

object main {
    def main(arg: Array[String]) = {        
      
      val cCodeDir = new File("C:\\Scala\\sandel_baseline\\src")
      for (cCodeFile <- cCodeDir.listFiles().filter(_.getName.endsWith(".h"))) {
        val fileName = cCodeFile.getName
        val fileNameWithoutExtension = cCodeFile.getName.substring(0, cCodeFile.getName.lastIndexOf('.'))
        val rt = Runtime.getRuntime();
        val proc = rt.exec("cmd /c gcc -E -P " + cCodeFile.getAbsolutePath + " > preprocessed_" + fileName)
        
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
        val listener = new MyListener();
        ParseTreeWalker.DEFAULT.walk(listener, ctx); 
  
        println("RESULTS: ")
        val resultWriter = new PrintWriter(new FileOutputStream("convertedCode\\" + fileNameWithoutExtension + ".scala"))
        listener.results.foreach{line => resultWriter.println(line)}
        resultWriter.flush
        resultWriter.close
        
        val preprocessedFile = new File("preprocessed_" + fileName)
        preprocessedFile.delete()
      }
    }   
}