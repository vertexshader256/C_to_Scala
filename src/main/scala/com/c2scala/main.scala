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

class MyListener extends CBaseListener {
  var isWithinStruct = false
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  var specifierQualifierLevel = 0
  var currentTypeName = ""
  val results = ListBuffer[String]()
  var isTypeEnum = false
  var isWithinFunction = false
  var hasTypedefName = false
  
  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpecifier = ""
  
  def convertTypeSpecifier(typeSpecifier: String) = typeSpecifier match {
    case "char" => "Char"
    case "float" => "Float"
    case "long" => "Long"
    case "short" => "Short"
    case "int" => "Integer"
    case _ => typeSpecifier
  }
  
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
  
  override def enterInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    isWithinFunction = true
  }
  
  override def exitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    //isWithinStruct = false
  }
  
  override def enterEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    isTypeEnum = true
  }
  
  override def enterDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    
  }
  
  override def exitDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    if (declarationHasStruct && !isWithinStruct) {
      var result = "case class " + typedefNames(0) + "(\n"
      //structDeclarations.foreach(println)
      if (!structDeclarations.isEmpty) {
        result += structDeclarations.map("  " + _).reduce{_ + ",\n" + _}
      }
      result += "\n)"
      results += result
    } else if (!isTypeEnum && !isWithinFunction && typedefNames.size == 1) {
      results += "type " + typedefNames(0) + " = " + convertTypeSpecifier(latestTypeSpecifier) + "\n"
    } else if (!isTypeEnum && !isWithinFunction && typedefNames.size == 2) {
      results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
    }
  }
  
  override def enterTypedefName(ctx: CParser.TypedefNameContext) = {
    
  }
  
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (isWithinStruct) {
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.Identifier().getText
      } else if (specifierQualifierLevel == 2) {
        structDeclarations += "var " + ctx.Identifier().getText + ": " + currentTypeName
      }
    } else {
      typedefNames += ctx.Identifier().getText
    }
    
    hasTypedefName = true
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("ENTER TYPE ")
    hasTypedefName = false
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("EXIT TYPE ")
    if (!hasTypedefName)
      latestTypeSpecifier = ctx.getText
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    declarationHasStruct = false
    isTypeEnum = false
    isWithinFunction = false
    typedefNames.clear
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    declarationHasStruct = false
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
        val listener = new MyListener();
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